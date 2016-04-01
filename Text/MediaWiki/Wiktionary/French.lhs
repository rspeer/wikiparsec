> {-# LANGUAGE OverloadedStrings #-}
> module Text.MediaWiki.Wiktionary.French where
> import Text.MediaWiki.Templates
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AList (get, filterEmpty, lookupOne, getOne, getDefault, ByteAssoc)
> import Text.MediaWiki.AnnotatedString (AnnotatedString, Annotation)
> import Text.MediaWiki.SplitUtils
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Sections
> import Text.MediaWiki.Wiktionary.Base
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as Char8
> import qualified Data.ByteString.Lazy.Char8 as LChar8
> import qualified Data.Aeson as Ae
> import Data.Attoparsec.ByteString.Char8
> import Data.LanguageNames
> import Control.Applicative ((<|>), (<$>), (*>), (<*))
> import Control.Monad


Parsing sections
================

> frHandlePage :: ByteString -> ByteString -> [WiktionaryRel]
> frHandlePage title text =
>   let sections = parsePageIntoSections text in
>     concat (map (frDispatchSection title) sections)
>
> frHandleFile :: String -> String -> IO ()
> frHandleFile title filename = do
>   contents <- Char8.readFile filename
>   mapM_ (LChar8.putStrLn . Ae.encode) (frHandlePage (Char8.pack title) contents)

Choosing an appropriate section parser
--------------------------------------

> fakeRel :: ByteString -> ByteString -> WiktionaryRel
> fakeRel s1 s2 = makeRel "debug" (simpleTerm "fr" s1) (simpleTerm "fr" s2)
>
> frDispatchSection :: ByteString -> WikiSection -> [WiktionaryRel]
> frDispatchSection title (WikiSection {headings=headings, content=content}) =
>   let evalHeadings = map evalHeading headings in
>     case frGetTerm title evalHeadings of
>       Nothing   -> []
>       Just term ->
>         let sectionType = frGetSectionType evalHeadings in
>           frParseSectionContent sectionType term content
>
> frParseSectionContent :: ByteString -> WiktionaryTerm -> ByteString -> [WiktionaryRel]
> frParseSectionContent label
>   | label == "POS"                = frParseDefinition
>   | label == "traductions"        = frParseTranslations
>   | label == "synonymes"          = frParseRelation "synonym"
>   | label == "quasi-synonymes"    = frParseRelation "quasi-synonym"
>   | label == "antonymes"          = frParseRelation "antonym"
>   | label == "hyponymes"          = frParseRelation "hyponym"
>   | label == "hyperonymes"        = frParseRelation "hypernym"
>   | label == (utf8 "méronymes")   = frParseRelation "meronym"
>   | label == "holonymes"          = frParseRelation "holonym"
>   | label == "troponymes"         = frParseRelation "troponym"
>   | label == "augmentatifs"       = frParseRelation "augmentative"
>   | label == "diminutifs"         = frParseRelation "diminutive"
>   | label == (utf8 "apparentés")  = frParseRelation "related"
>   | label == (utf8 "dérivés")     = frParseRelation "derived"
>   | label == (utf8 "dérivés autres langues") = frParseRelation "derived"
>   | label == "drv-int"            = frParseRelation "derived"
>   | label == "variantes"          = frParseRelation "variant"
>   | label == "variantes orthographiques" = frParseRelation "variant"
>   | label == "var-ortho"          = frParseRelation "variant"
>   | otherwise                     = const (const [])


The part-of-speech/definition section
-------------------------------------

First, make a specific version of the function that extracts relationships
from the text of a definition:

> frDefinitionToRels = definitionToRels "fr"

Parsing the definition section:

> frParseDefinition :: WiktionaryTerm -> ByteString -> [WiktionaryRel]
> frParseDefinition thisTerm text =
>   case parseOnly pDefinitionSection text of
>     Left err   -> error (show text)
>     Right defs -> concat (map (definitionToRels "fr" thisTerm) defs)
>
> pDefinitionSection :: Parser [(ByteString, AnnotatedString)]
> pDefinitionSection =
>   -- Skip miscellaneous lines at the start of the section: try to parse
>   -- each line as pDefinitionList, and if that fails, parse one line,
>   -- throw it out, and parse the rest recursively.
>   pDefinitionList <|>
>   (newLine >> pDefinitionSection) <|>
>   (wikiTextLine noTemplates >> newLine >> pDefinitionSection)
>
> pDefinitionList :: Parser [(ByteString, AnnotatedString)]
> pDefinitionList = extractNumberedDefs <$> orderedList frTemplates "#"


The translation section
-----------------------

> frParseTranslations :: WiktionaryTerm -> ByteString -> [WiktionaryRel]
> frParseTranslations thisTerm text =
>   case parseOnly (pTranslationSection thisTerm) text of
>     Left err -> error (show text)
>     Right val -> val
>
> pTranslationSection :: WiktionaryTerm -> Parser [WiktionaryRel]
> pTranslationSection thisTerm = concat <$> many1 (pTranslationGroup thisTerm)
>
> pTranslationGroup :: WiktionaryTerm -> Parser [WiktionaryRel]
> pTranslationGroup thisTerm = do
>   optionalTextChoices [newLine]
>   maybeSense <- pTransTop
>   let senseTerm = thisTerm {sense=maybeSense}
>   items <- pTranslationList
>   optionalTextChoices [newLine]
>   return (map (annotationToRel senseTerm) (filter translationsOnly items))
>
> translationsOnly :: Annotation -> Bool
> translationsOnly annot = (get "rel" annot) == "translation"

The `pTranslationTopTemplate` rule parses the template that starts a
translation section, which may or may not be labeled with a word sense. It
returns a Maybe ByteString that contains the word sense if present.

> pTransTop = do
>   template <- specificTemplate frTemplates (utf8 "trad-début")
>   newLine
>   return (lookup "1" template)

> pTranslationList :: Parser [Annotation]
> pTranslationList = concat <$> many1 (pTranslationItem <|> pTranslationBlankLine) <* pTranslationListEnd
> pTranslationListEnd = specificTemplate frTemplates "trad-fin" <* many1 newLine

The procedure for getting translations out of a bunch of bullet points involved a few
chained procedures, which of course occur from right to left:

  - Parse a bullet-pointed list entry.

  - Find the items the bullet-pointed list entry contains. There may be
    multiple of them, because some translation entries are nested lists --
    multiple kinds of translations for the same language, for example.
    `extractTopLevel` turns these items into a flat list.

  - `extractTopLevel` gave us a list of AnnotatedStrings. We want just their
    annotations, representing everything we want to know about the
    translations, in one big list. So we `A.concat` all the AnnotatedStrings
    together, and then take the combined list of annotations from that.

> pTranslationItem :: Parser [Annotation]
> pTranslationItem = A.annotations <$> A.concat <$> extractTopLevel <$> listItem frTemplates "*"
>
> pTranslationBlankLine :: Parser [Annotation]
> pTranslationBlankLine = newLine >> return []


Relation sections
-----------------

> frParseRelation :: ByteString -> WiktionaryTerm -> ByteString -> [WiktionaryRel]
> frParseRelation rel thisTerm text = parseOrDefault [] (pRelationSection rel thisTerm) text
>
> pRelationSection :: ByteString -> WiktionaryTerm -> Parser [WiktionaryRel]
> pRelationSection rel thisTerm = map (assignRel rel)
>                                 <$> concat
>                                 <$> map (entryToRels thisTerm)
>                                 <$> extractTextLines
>                                 <$> bulletList frTemplates "*"


Labels
------

Here's the part of the code that involves pasting in large amounts of trivia
from category pages.

Every domain that would be labeled in English using the {{label}} template has
its own template in French. Here's a list of all of them, taken from
https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_domaine_d’utilisation .

> domainLabels :: [ByteString]
> domainLabels = [
>   utf8 "acoustique",
>   utf8 "administration",
>   utf8 "aéronautique",
>   utf8 "agriculture",
>   utf8 "aïkido",
>   utf8 "alchimie",
>   utf8 "alcools",
>   utf8 "algues",
>   utf8 "algèbre",
>   utf8 "aliments",
>   utf8 "alliages",
>   utf8 "alpinisme",
>   utf8 "analyse",
>   utf8 "anatomie",
>   utf8 "anciennes divisions",
>   utf8 "anciennes localités",
>   utf8 "anglicismes informatiques",
>   utf8 "anglicismes militaires",
>   utf8 "animaux",
>   utf8 "anthropologie",
>   utf8 "antilopes",
>   utf8 "Antiquité",
>   utf8 "apiculture",
>   utf8 "arbres",
>   utf8 "arbres fruitiers",
>   utf8 "archéologie",
>   utf8 "architecture",
>   utf8 "architecture des ordinateurs",
>   utf8 "Argadz",
>   utf8 "armement",
>   utf8 "armes",
>   utf8 "armes blanches",
>   utf8 "arthropodes",
>   utf8 "arts",
>   utf8 "arts martiaux",
>   utf8 "assurance",
>   utf8 "astrologie",
>   utf8 "astronautique",
>   utf8 "astronomie",
>   utf8 "astrophysique",
>   utf8 "athlétisme",
>   utf8 "audiovisuel",
>   utf8 "automatique",
>   utf8 "automobile",
>   utf8 "auxiliaire",
>   utf8 "aviation",
>   utf8 "avions",
>   utf8 "bactériologie",
>   utf8 "badminton",
>   utf8 "base de données",
>   utf8 "baseball",
>   utf8 "basket-ball",
>   utf8 "bateaux",
>   utf8 "BD",
>   utf8 "beaux-arts",
>   utf8 "bibliothéconomie",
>   utf8 "bijouterie",
>   utf8 "billard",
>   utf8 "biochimie",
>   utf8 "biologie",
>   utf8 "biophysique",
>   utf8 "boissons",
>   utf8 "botanique",
>   utf8 "boucherie",
>   utf8 "bouddhisme",
>   utf8 "bowling",
>   utf8 "boxe",
>   utf8 "bridge",
>   utf8 "calendrier",
>   utf8 "canards",
>   utf8 "canoë-kayak",
>   utf8 "capoeira",
>   utf8 "carnivore",
>   utf8 "cartes",
>   utf8 "caséologie",
>   utf8 "CB",
>   utf8 "cépages",
>   utf8 "céphalopodes",
>   utf8 "céramique",
>   utf8 "cétacés",
>   utf8 "chaînes de montagnes",
>   utf8 "champignons",
>   utf8 "charpenterie",
>   utf8 "charronnerie",
>   utf8 "chasse",
>   utf8 "chats",
>   utf8 "chaussures",
>   utf8 "chiens",
>   utf8 "chimie",
>   utf8 "chimie organique",
>   utf8 "chiromancie",
>   utf8 "chirurgie",
>   utf8 "christianisme",
>   utf8 "cinéma",
>   utf8 "cocktails",
>   utf8 "coiffure",
>   utf8 "coléoptères",
>   utf8 "colorimétrie",
>   utf8 "commerce",
>   utf8 "commerces",
>   utf8 "composants électriques",
>   utf8 "composants électroniques",
>   utf8 "comptabilité",
>   utf8 "condiments",
>   utf8 "confiseries",
>   utf8 "conifères",
>   utf8 "construction",
>   utf8 "cordonnerie",
>   utf8 "cosmétologie",
>   utf8 "couche application",
>   utf8 "couche liaison",
>   utf8 "couche physique",
>   utf8 "couche présentation",
>   utf8 "couche réseau",
>   utf8 "couche session",
>   utf8 "couche transport",
>   utf8 "cours d’eau",
>   utf8 "course à pied",
>   utf8 "couture",
>   utf8 "couvertures",
>   utf8 "couvre-chefs",
>   utf8 "créatures",
>   utf8 "cricket",
>   utf8 "crimes",
>   utf8 "crustacés",
>   utf8 "cryptographie",
>   utf8 "cuisine",
>   utf8 "cyclisme",
>   utf8 "céréales",
>   utf8 "dames",
>   utf8 "danse",
>   utf8 "danses",
>   utf8 "death metal",
>   utf8 "dentisterie",
>   utf8 "départements",
>   utf8 "dermatologie",
>   utf8 "desserts",
>   utf8 "dessin",
>   utf8 "dialectes",
>   utf8 "didactique",
>   utf8 "dindons",
>   utf8 "dinosaures",
>   utf8 "diplomatie",
>   utf8 "distinctions",
>   utf8 "drogues",
>   utf8 "droit",
>   utf8 "droit du travail",
>   utf8 "échafaudage",
>   utf8 "échecs",
>   utf8 "biogéographie",
>   utf8 "écologie",
>   utf8 "économie",
>   utf8 "édifices",
>   utf8 "édition",
>   utf8 "éducation",
>   utf8 "électricité",
>   utf8 "électronique",
>   utf8 "électrotechnique",
>   utf8 "éléments",
>   utf8 "élevage",
>   utf8 "embryologie",
>   utf8 "entomologie",
>   utf8 "enzymes",
>   utf8 "épices",
>   utf8 "équitation",
>   utf8 "escrime",
>   utf8 "états",
>   utf8 "ethnologie",
>   utf8 "ethnonymes",
>   utf8 "étoiles",
>   utf8 "famille",
>   utf8 "fantastique",
>   utf8 "fauconnerie",
>   utf8 "félins",
>   utf8 "ferroviaire",
>   utf8 "figures",
>   utf8 "finance",
>   utf8 "fiscalité",
>   utf8 "fleurs",
>   utf8 "fontainerie",
>   utf8 "football",
>   utf8 "foresterie",
>   utf8 "franc-maçonnerie",
>   utf8 "fromages",
>   utf8 "fruits",
>   utf8 "gâteaux",
>   utf8 "généalogie",
>   utf8 "génétique",
>   utf8 "genres musicaux",
>   utf8 "gentilés",
>   utf8 "géographie",
>   utf8 "géologie",
>   utf8 "géomatique",
>   utf8 "géométrie",
>   utf8 "géophysique",
>   utf8 "géostatistique",
>   utf8 "glaciologie",
>   utf8 "gladiateurs",
>   utf8 "golf",
>   utf8 "golfes",
>   utf8 "grades",
>   utf8 "grammaire",
>   utf8 "gravure",
>   utf8 "gymnastique",
>   utf8 "handball",
>   utf8 "héraldique",
>   utf8 "hindouisme",
>   utf8 "hippisme",
>   utf8 "hippologie",
>   utf8 "histoire",
>   utf8 "histologie",
>   utf8 "hockey",
>   utf8 "horlogerie",
>   utf8 "horticulture",
>   utf8 "hydraulique",
>   utf8 "ichtyologie",
>   utf8 "îles",
>   utf8 "illégalité",
>   utf8 "imprimerie",
>   utf8 "industrie",
>   utf8 "industrie pétrolière",
>   utf8 "infographie",
>   utf8 "informatique",
>   utf8 "insectes",
>   utf8 "instruments",
>   utf8 "instruments de mesure",
>   utf8 "insultes",
>   utf8 "intelligence artificielle",
>   utf8 "Internet",
>   utf8 "islam",
>   utf8 "jardinage",
>   utf8 "jazz",
>   utf8 "jeu de paume",
>   utf8 "jeux",
>   utf8 "jeux de cartes",
>   utf8 "jeux vidéo",
>   utf8 "jonglerie",
>   utf8 "jouets",
>   utf8 "journalisme",
>   utf8 "judaïsme",
>   utf8 "judo",
>   utf8 "justice",
>   utf8 "karaté",
>   utf8 "langage Java",
>   utf8 "langages",
>   utf8 "langues",
>   utf8 "législation",
>   utf8 "légumes",
>   utf8 "lexicographie",
>   utf8 "lézards",
>   utf8 "LGBT",
>   utf8 "lianes",
>   utf8 "linguistique",
>   utf8 "littérature",
>   utf8 "liturgie",
>   utf8 "livre",
>   utf8 "localités",
>   utf8 "logique",
>   utf8 "logistique",
>   utf8 "loisirs",
>   utf8 "lutherie",
>   utf8 "léporidé",
>   utf8 "machines",
>   utf8 "maçonnerie",
>   utf8 "magnétisme",
>   utf8 "mah-jong",
>   utf8 "maintenance",
>   utf8 "maladies",
>   utf8 "maladies de l’œil",
>   utf8 "mammifères",
>   utf8 "management",
>   utf8 "marbrerie",
>   utf8 "maréchalerie",
>   utf8 "marine",
>   utf8 "maroquinerie",
>   utf8 "marsupial",
>   utf8 "mathématiques",
>   utf8 "probabilités",
>   utf8 "mécanique",
>   utf8 "médecine",
>   utf8 "médecine non conv",
>   utf8 "médecine vétérinaire",
>   utf8 "média",
>   utf8 "médicaments",
>   utf8 "menuiserie",
>   utf8 "mercatique",
>   utf8 "métallurgie",
>   utf8 "métaplasmes",
>   utf8 "météorologie",
>   utf8 "scientifiques",
>   utf8 "métrologie",
>   utf8 "meubles",
>   utf8 "microbiologie",
>   utf8 "militaire",
>   utf8 "minéralogie",
>   utf8 "minéraux",
>   utf8 "miroiterie",
>   utf8 "mollusques",
>   utf8 "monarchie",
>   utf8 "monnaies",
>   utf8 "montagnes",
>   utf8 "motocyclisme",
>   utf8 "Moyen Âge",
>   utf8 "muscle",
>   utf8 "muscles",
>   utf8 "musculation",
>   utf8 "musiciens",
>   utf8 "musique",
>   utf8 "musiques",
>   utf8 "mycologie",
>   utf8 "mythologie",
>   utf8 "narratologie",
>   utf8 "natation",
>   utf8 "navigation",
>   utf8 "neurologie",
>   utf8 "noblesse",
>   utf8 "nosologie",
>   utf8 "novlangue",
>   utf8 "nucléaire",
>   utf8 "numismatique",
>   utf8 "nutrition",
>   utf8 "oenologie",
>   utf8 "oiseaux",
>   utf8 "optique",
>   utf8 "optométrie",
>   utf8 "ornement",
>   utf8 "ornithologie",
>   utf8 "un os",
>   utf8 "outils",
>   utf8 "paléographie",
>   utf8 "paléontologie",
>   utf8 "palmier",
>   utf8 "papeterie",
>   utf8 "papillons",
>   utf8 "pâtes alimentaires",
>   utf8 "patinage",
>   utf8 "pâtisserie",
>   utf8 "pays",
>   utf8 "pêche",
>   utf8 "pêches",
>   utf8 "pédologie",
>   utf8 "peinture",
>   utf8 "pelote",
>   utf8 "pétanque",
>   utf8 "pétrochimie",
>   utf8 "pharmacologie",
>   utf8 "philatélie",
>   utf8 "philosophie",
>   utf8 "phobies",
>   utf8 "phonétique",
>   utf8 "phonétique classique",
>   utf8 "photographie",
>   utf8 "physiologie",
>   utf8 "physique",
>   utf8 "planche à roulettes",
>   utf8 "plantes",
>   utf8 "plantes aromatiques",
>   utf8 "plomberie",
>   utf8 "plongée",
>   utf8 "poésie",
>   utf8 "points cardinaux",
>   utf8 "poires",
>   utf8 "poissons",
>   utf8 "poker",
>   utf8 "police",
>   utf8 "politique",
>   utf8 "pommes",
>   utf8 "ponctuations",
>   utf8 "positions",
>   utf8 "préhistoire",
>   utf8 "préparations",
>   utf8 "primates",
>   utf8 "programmation",
>   utf8 "programmation orientée objet",
>   utf8 "protéines",
>   utf8 "protocoles",
>   utf8 "proverbes",
>   utf8 "provinces",
>   utf8 "psychiatrie",
>   utf8 "psychologie",
>   utf8 "quartiers",
>   utf8 "régions",
>   utf8 "religieux",
>   utf8 "religion",
>   utf8 "religions",
>   utf8 "reliure",
>   utf8 "reproduction",
>   utf8 "reptiles",
>   utf8 "requins",
>   utf8 "réseaux informatiques",
>   utf8 "rhétorique",
>   utf8 "robotique",
>   utf8 "roches",
>   utf8 "rongeur",
>   utf8 "rugby",
>   utf8 "salades",
>   utf8 "saliculture",
>   utf8 "sandwichs",
>   utf8 "satellites",
>   utf8 "science-fiction",
>   utf8 "sciences",
>   utf8 "Scrabble",
>   utf8 "sculpture",
>   utf8 "seigneuries",
>   utf8 "sentiments",
>   utf8 "serpents",
>   utf8 "serrurerie",
>   utf8 "sexualité",
>   utf8 "sidérurgie",
>   utf8 "singes",
>   utf8 "ski alpin",
>   utf8 "ski de fond",
>   utf8 "snowboard",
>   utf8 "snowboard/Documentation",
>   utf8 "sociolinguistique",
>   utf8 "sociologie",
>   utf8 "soldats",
>   utf8 "sport",
>   utf8 "sport de combat",
>   utf8 "sport de glisse",
>   utf8 "sportifs",
>   utf8 "sports",
>   utf8 "sports de combat",
>   utf8 "sports de glisse",
>   utf8 "squelette",
>   utf8 "statistiques",
>   utf8 "stéréotomie",
>   utf8 "substances",
>   utf8 "surf",
>   utf8 "sylviculture",
>   utf8 "électoraux",
>   utf8 "taille de pierre",
>   utf8 "tauromachie",
>   utf8 "technique",
>   utf8 "technologie",
>   utf8 "télécommunications",
>   utf8 "téléinformatique",
>   utf8 "téléphonie",
>   utf8 "temps géologiques",
>   utf8 "tennis",
>   utf8 "textile",
>   utf8 "textiles",
>   utf8 "théâtre",
>   utf8 "théologie",
>   utf8 "théorie des graphes",
>   utf8 "thermodynamique",
>   utf8 "tonnellerie",
>   utf8 "topographie",
>   utf8 "topologie",
>   utf8 "toponymes",
>   utf8 "toponymie",
>   utf8 "tourisme",
>   utf8 "transport",
>   utf8 "travail",
>   utf8 "typographie",
>   utf8 "unités",
>   utf8 "symboles unités",
>   utf8 "urbanisme",
>   utf8 "usinage",
>   utf8 "ustensiles",
>   utf8 "vaudou",
>   utf8 "véhicules",
>   utf8 "vents",
>   utf8 "versification",
>   utf8 "vêtements",
>   utf8 "viandes",
>   utf8 "vins",
>   utf8 "virologie",
>   utf8 "virus",
>   utf8 "viticulture",
>   utf8 "vitrerie",
>   utf8 "voitures",
>   utf8 "wiki",
>   utf8 "yoga",
>   utf8 "zoologie"]

Regional labels, from https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_pays_du_Wiktionnaire
and https://fr.wiktionary.org/wiki/Catégorie:Modèles_régionaux_du_Wiktionnaire:

> regionLabels = [
>   utf8 "Acadie",
>   utf8 "Afrique",
>   utf8 "Afrique du Sud",
>   utf8 "Algérie",
>   utf8 "Allemagne",
>   utf8 "Alsace",
>   utf8 "Amérique centrale",
>   utf8 "Amérique du Nord",
>   utf8 "Amérique du Sud",
>   utf8 "Amérique latine",
>   utf8 "Andorre",
>   utf8 "Anjou",
>   utf8 "Antilles",
>   utf8 "Aquitaine",
>   utf8 "Argentine",
>   utf8 "Australie",
>   utf8 "Autriche",
>   utf8 "Auvergne",
>   utf8 "Baléares",
>   utf8 "Belgique",
>   utf8 "Luxembourg",
>   utf8 "Belize",
>   utf8 "Bénin",
>   utf8 "Berry",
>   utf8 "Bolivie",
>   utf8 "Bordelais",
>   utf8 "Bourgogne",
>   utf8 "Bretagne",
>   utf8 "Brésil",
>   utf8 "Burkina Faso",
>   utf8 "Cameroun",
>   utf8 "Canada",
>   utf8 "Catalogne",
>   utf8 "Champagne",
>   utf8 "Chili",
>   utf8 "Chine",
>   utf8 "Colombie",
>   utf8 "Commonwealth",
>   utf8 "Congo",
>   utf8 "Congo-Brazzaville",
>   utf8 "Congo-Kinshasa",
>   utf8 "Corse",
>   utf8 "Corée du Nord",
>   utf8 "Corée du Sud",
>   utf8 "Costa Rica",
>   utf8 "Cuba",
>   utf8 "Côte d’Ivoire",
>   utf8 "Écosse",
>   utf8 "Espagne",
>   utf8 "Émirats arabes unis",
>   utf8 "États-Unis",
>   utf8 "Europe",
>   utf8 "France",
>   utf8 "Franche-Comté",
>   utf8 "Gascogne",
>   utf8 "Gaspésie",
>   utf8 "Guadeloupe",
>   utf8 "Guinée",
>   utf8 "Guyane",
>   utf8 "Haïti",
>   utf8 "Honduras",
>   utf8 "Île-de-France",
>   utf8 "Inde",
>   utf8 "Irlande",
>   utf8 "Jamaïque",
>   utf8 "Japon",
>   utf8 "Kirghizistan",
>   utf8 "Languedoc",
>   utf8 "Languedoc-Roussillon",
>   utf8 "Le Mans",
>   utf8 "Liban",
>   utf8 "Liechtenstein",
>   utf8 "Limousin",
>   utf8 "Lorraine",
>   utf8 "Louisiane",
>   utf8 "Lyonnais",
>   utf8 "Madagascar",
>   utf8 "Maghreb",
>   utf8 "Malaisie",
>   utf8 "Mali",
>   utf8 "Maroc",
>   utf8 "Marseille",
>   utf8 "Martinique",
>   utf8 "Maurice",
>   utf8 "Mayotte",
>   utf8 "Midi",
>   utf8 "Midi toulousain",
>   utf8 "Moldavie",
>   utf8 "Mongolie",
>   utf8 "Namibie",
>   utf8 "Nantes",
>   utf8 "Nicaragua",
>   utf8 "Niger",
>   utf8 "Nigeria",
>   utf8 "Nord-Pas-de-Calais",
>   utf8 "Normandie",
>   utf8 "Nouvelle-Calédonie",
>   utf8 "Nouvelle-Zélande",
>   utf8 "Occitanie",
>   utf8 "Océanie",
>   utf8 "Ouzbékistan",
>   utf8 "Panama",
>   utf8 "Paraguay",
>   utf8 "Paris",
>   utf8 "Pays-Bas",
>   utf8 "Pays basque",
>   utf8 "Picardie",
>   utf8 "Poitou",
>   utf8 "Polynésie française",
>   utf8 "Portugal",
>   utf8 "Provence",
>   utf8 "Pyrénées-Orientales",
>   utf8 "Pérou",
>   utf8 "Québec",
>   utf8 "Quercy",
>   utf8 "République dominicaine",
>   utf8 "Réunion",
>   utf8 "Rhône-Alpes",
>   utf8 "Roumanie",
>   utf8 "Royaume-Uni",
>   utf8 "République Démocratique du Congo",
>   utf8 "Salvador",
>   utf8 "Savoie",
>   utf8 "Suède",
>   utf8 "Suisse",
>   utf8 "Sénégal",
>   utf8 "Taïwan",
>   utf8 "Tchad",
>   utf8 "Terre-Neuve",
>   utf8 "Togo",
>   utf8 "Transnistrie",
>   utf8 "Tunisie",
>   utf8 "Uruguay",
>   utf8 "Var",
>   utf8 "Velay",
>   utf8 "Vendée",
>   utf8 "Venezuela",
>   utf8 "Viêt Nam"]

Other relevant labels from https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_contexte:

> otherLabels = [
>   utf8 "archaïque",
>   utf8 "argot",  -- see also the template rule that handles templates starting with "argot"
>   utf8 "désuet",
>   utf8 "vulgaire"]
>
> contextLabels = domainLabels ++ regionLabels ++ otherLabels


> handleContextTemplate :: Template -> AnnotatedString
> handleContextTemplate template =
>   A.singleAnnotation [("rel", "context"), ("language", "fr"), ("page", get "0" template)]
>
> handleMiscContextTemplate template =
>   A.singleAnnotation [("rel", "context"), ("language", "fr"), ("page", get "1" template)]


Finding headings
================

> partOfSpeechMap :: ByteString -> ByteString
> partOfSpeechMap pos
>   | Char8.isPrefixOf "adj" pos = "a"
>   | Char8.isPrefixOf "adv" pos = "r"
>   | Char8.isPrefixOf "nom" pos = "n"
>   | Char8.isPrefixOf "pronom" pos = "n"
>   | Char8.isPrefixOf "verb" pos = "v"
>   | otherwise = "_"

Generalizing the type of a heading:

> frGetSectionType :: [Annotation] -> ByteString
> frGetSectionType headings =
>   let thisHead    = last headings
>       headingText = get "text" thisHead
>   in if (length headings) == 3
>        then (if headingText == "POS" then "POS" else "?")
>        else headingText

Figure out what term we're talking about from the values of the templates
in the section headings:

> frGetTerm :: ByteString -> [Annotation] -> Maybe WiktionaryTerm
> frGetTerm title headingValues =
>   if (length headingValues) < 3 || get "text" (headingValues !! 2) /= "POS"
>     then Nothing
>     else
>       let language = get "text" (headingValues !! 1)
>           pos      = get "pos" (headingValues !! 2)
>           etym     = get "etym" (headingValues !! 2)
>       in Just (WiktionaryTerm { text=title, language=Just language, etym=Just etym, pos=Just pos, sense=Nothing })

Instead of using an AnnotatedString with many possible annotations, we'll
represent a heading as a single annotation (a single mapping from bytestrings
to bytestrings). To preserve the information, we add one more key, "text", to
the annotation.

> evalHeading :: ByteString -> Annotation
> evalHeading heading =
>   case parseOnly (annotatedWikiText frTemplates) heading of
>     Left err      -> error err
>     Right astring ->
>       let text  = A.unannotate astring
>           annos = A.annotations astring
>           anno  = if annos == [] then [] else (annos !! 0)
>       in ("text", text):anno
>


Evaluating templates
====================

Section headings
----------------

Handle the {{S}} template that starts many sections. If it has a second
argument, it's probably a part-of-speech template (we'll also check that it
occurs at the right level of the section hierarchy). Otherwise, it introduces
a section named for its first argument.

> handleSectionTemplate :: Template -> AnnotatedString
> handleSectionTemplate template =
>   case (lookup "2" template) of
>     Just lang -> A.annotate [[("pos", get "1" template), ("etym", getDefault "1" "num" template)]] "POS"
>     Nothing   -> A.annotate [] (get "1" template)

Language sections start with a {{langue}} template that usually just contains
the BCP 47 language code, which is what we want.

> handleLanguageCodeTemplate :: Template -> AnnotatedString
> handleLanguageCodeTemplate template =
>   case (get "1" template) of
>     -- French labels their translingual entries "conv", for "convention".
>     -- We want to label them with the BCP 47 code "mul".
>     "conv" -> A.fromBytes "mul"
>     x      -> A.fromBytes x

Links
-----

> handleLinkTemplate :: Template -> AnnotatedString
> handleLinkTemplate template =
>   let text  = (getOne ["dif", "1"] template)
>       annot = filterEmpty $
>         [("language", (get "2" template)),
>          ("page", (get "1" template)),
>          ("sense", (get "sens" template))]
>   in (A.annotate [annot] text)


Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedString
> handleTranslationTemplate template =
>   let annot = filterEmpty $
>         [("rel", "translation"),
>          ("language", (get "1" template)),
>          ("page", (get "2" template))]
>       text  = getOne ["dif", "2"] template
>   in A.annotate [annot] text


Putting it all together
-----------------------

> frTemplates :: TemplateProc
> frTemplates "S"      = handleSectionTemplate
> frTemplates "langue" = handleLanguageCodeTemplate
> frTemplates "term"   = handleMiscContextTemplate
> frTemplates "lien"   = handleLinkTemplate
> frTemplates "lien m" = handleLinkTemplate
> frTemplates "trad"   = handleTranslationTemplate
> frTemplates "trad+"  = handleTranslationTemplate
> frTemplates "trad-"  = handleTranslationTemplate
> frTemplates "trad--" = handleTranslationTemplate

Cases that need to be checked with expressions instead of plain pattern
matches:

> frTemplates x
>   -- if the template name is in the long list of contexts, use it
>   -- as a context label
>   | elem x contextLabels       = handleContextTemplate
>   | Char8.isPrefixOf "argot" x = handleContextTemplate
>   | otherwise                  = skipTemplate

