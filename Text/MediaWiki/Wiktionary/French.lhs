> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Export only the top-level, namespaced functions.

> module Text.MediaWiki.Wiktionary.French
>   (frParseWiktionary, frTemplates) where
> import WikiPrelude
> import Text.MediaWiki.Templates
> import Text.MediaWiki.AnnotatedText
> import Text.MediaWiki.SplitUtils
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.Sections
>   (WikiSection(..), parsePageIntoSections, headings, content)
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Wiktionary.Base
> import Data.Attoparsec.Text
> import Data.LanguageNames


Parsing entire pages
====================

This function can be passed as an argument to `handleFile` in
Text.MediaWiki.Wiktionary.Base.

> frParseWiktionary :: Text -> Text -> [WiktionaryFact]
> frParseWiktionary title text =
>   let sections = parsePageIntoSections text in
>     concat (map (frParseSection title) sections)


Finding headings
================

Unlike the English Wiktionary, the French Wiktionary frequently uses templates
within headings. `evalHeading` will read a heading as WikiText, returning an
AnnotatedText object.

> evalHeading :: Text -> AnnotatedText
> evalHeading heading =
>   case parseOnly (annotatedWikiText frTemplates) heading of
>     Left err      -> error err
>     Right atext   -> atext

The section type is whatever text was returned when we parsed the section
template. (See the Templates section.)

> getSectionType :: [AnnotatedText] -> Text
> getSectionType headings =
>   case lastMay headings of
>     Nothing -> error "Empty list of headings"
>     Just thisHead -> getText thisHead

The `partOfSpeechMap` maps French terms for parts of speech (as they appear in
templates) to the simple set of POS symbols used in WordNet or ConceptNet.

> partOfSpeechMap :: Text -> Text
> partOfSpeechMap pos
>   | isPrefixOf "adj" pos = "a"
>   | isPrefixOf "adv" pos = "r"
>   | isPrefixOf "nom" pos = "n"
>   | isPrefixOf "pronom" pos = "n"
>   | isPrefixOf "verb" pos = "v"
>   | otherwise = "_"

Figure out what term we're talking about from the values of the templates
in the section headings:

> getTerm :: Text -> [AnnotatedText] -> Maybe WiktionaryTerm
> getTerm title headingValues =
>   if (length headingValues) < 3 || getTextInList 2 headingValues /= Just "POS"
>     then Nothing
>     else do
>       language <- getTextInList 1 headingValues
>       pos <- getAnnotationInList 2 "pos" headingValues
>       -- use pattern-matching on "etym" because it's allowed to be Nothing
>       case getAnnotationInList 2 "etym" headingValues of
>         Just etym -> return (term [title, language, pos, etym])
>         Nothing   -> return (term [title, language, pos])
>


Parsing sections
================

Choosing an appropriate section parser
--------------------------------------

`frParseSection` takes in a title and a WikiSection structure, builds
a WiktionaryTerm structure for the term we're defining, and passes it on to
a function that will extract WiktionaryFacts.

> frParseSection :: Text -> WikiSection -> [WiktionaryFact]
> frParseSection title (WikiSection {headings=headings, content=content}) =
>   let evalHeadings = map evalHeading headings in
>     case getTerm title evalHeadings of
>       Nothing   -> []
>       Just term ->
>         let sectionType = getSectionType evalHeadings in
>           chooseSectionParser sectionType term content
>
> chooseSectionParser :: Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> chooseSectionParser "POS"             = frParseDefinitions
> chooseSectionParser "traductions"     = frParseTranslations
> chooseSectionParser "synonymes"       = frParseRelation "synonym"
> chooseSectionParser "quasi-synonymes" = frParseRelation "quasi-synonym"
> chooseSectionParser "antonymes"       = frParseRelation "antonym"
> chooseSectionParser "hyponymes"       = frParseRelation "hyponym"
> chooseSectionParser "hyperonymes"     = frParseRelation "hypernym"
> chooseSectionParser "méronymes"       = frParseRelation "meronym"
> chooseSectionParser "holonymes"       = frParseRelation "holonym"
> chooseSectionParser "troponymes"      = frParseRelation "troponym"
> chooseSectionParser "augmentatifs"    = frParseRelation "augmentative"
> chooseSectionParser "diminutifs"      = frParseRelation "diminutive"
> chooseSectionParser "apparentés"      = frParseRelation "related"
> chooseSectionParser "derivés"         = frParseRelation "derived"
> chooseSectionParser "derivés autres langues" = frParseRelation "derived"
> chooseSectionParser "drv-int"         = frParseRelation "derived"
> chooseSectionParser "variantes"       = frParseRelation "variant"
> chooseSectionParser "variantes orthographiques" = frParseRelation "variant"
> chooseSectionParser "var-ortho"       = frParseRelation "variant"
> chooseSectionParser _                 = const (const [])


Defining section parsers
------------------------

Defining section parsers by filling in the missing details in more general
functions, defined in `Text.MediaWiki.Wiktionary.Base`:

> frParseDefinitions = parseDefinitions "fr" frTemplates
>
> frParseRelation = parseRelation $ RelationSectionInfo {
>   rsLanguage="fr",
>   rsTemplateProc=frTemplates,
>   rsItemRule=pRelationItem
> }
>
> frParseTranslations = parseTranslations $ TranslationSectionInfo {
>   tsLanguage="fr",
>   tsTemplateProc=frTemplates,
>   tsStartRule=pTransTop,
>   tsIgnoreRule=pBlankLine,
>   tsEndRule=pTransBottom
> }

The `pTranslationTopTemplate` rule parses the template that starts a
translation section. If this template has an argument, it labels the word
sense.

> pTransTop :: Parser (Maybe Text)
> pTransTop = do
>   template <- specificTemplate frTemplates "trad-début"
>   newLine
>   return (lookup "1" template)
>
> pTransBottom :: Parser ()
> pTransBottom = specificTemplate frTemplates "trad-fin" >> return ()


Labels
------

Here's the part of the code that involves pasting in large amounts of trivia
from category pages.

Every domain that would be labeled in English using the {{label}} template has
its own template in French. Here's a list of all of them, taken from
https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_domaine_d’utilisation .

> domainLabels :: HashSet Text
> domainLabels = setFromList [
>   "acoustique",
>   "administration",
>   "aéronautique",
>   "agriculture",
>   "aïkido",
>   "alchimie",
>   "alcools",
>   "algues",
>   "algèbre",
>   "aliments",
>   "alliages",
>   "alpinisme",
>   "analyse",
>   "anatomie",
>   "anciennes divisions",
>   "anciennes localités",
>   "anglicismes informatiques",
>   "anglicismes militaires",
>   "animaux",
>   "anthropologie",
>   "antilopes",
>   "Antiquité",
>   "apiculture",
>   "arbres",
>   "arbres fruitiers",
>   "archéologie",
>   "architecture",
>   "architecture des ordinateurs",
>   "Argadz",
>   "armement",
>   "armes",
>   "armes blanches",
>   "arthropodes",
>   "arts",
>   "arts martiaux",
>   "assurance",
>   "astrologie",
>   "astronautique",
>   "astronomie",
>   "astrophysique",
>   "athlétisme",
>   "audiovisuel",
>   "automatique",
>   "automobile",
>   "auxiliaire",
>   "aviation",
>   "avions",
>   "bactériologie",
>   "badminton",
>   "base de données",
>   "baseball",
>   "basket-ball",
>   "bateaux",
>   "BD",
>   "beaux-arts",
>   "bibliothéconomie",
>   "bijouterie",
>   "billard",
>   "biochimie",
>   "biologie",
>   "biophysique",
>   "boissons",
>   "botanique",
>   "boucherie",
>   "bouddhisme",
>   "bowling",
>   "boxe",
>   "bridge",
>   "calendrier",
>   "canards",
>   "canoë-kayak",
>   "capoeira",
>   "carnivore",
>   "cartes",
>   "caséologie",
>   "CB",
>   "cépages",
>   "céphalopodes",
>   "céramique",
>   "cétacés",
>   "chaînes de montagnes",
>   "champignons",
>   "charpenterie",
>   "charronnerie",
>   "chasse",
>   "chats",
>   "chaussures",
>   "chiens",
>   "chimie",
>   "chimie organique",
>   "chiromancie",
>   "chirurgie",
>   "christianisme",
>   "cinéma",
>   "cocktails",
>   "coiffure",
>   "coléoptères",
>   "colorimétrie",
>   "commerce",
>   "commerces",
>   "composants électriques",
>   "composants électroniques",
>   "comptabilité",
>   "condiments",
>   "confiseries",
>   "conifères",
>   "construction",
>   "cordonnerie",
>   "cosmétologie",
>   "couche application",
>   "couche liaison",
>   "couche physique",
>   "couche présentation",
>   "couche réseau",
>   "couche session",
>   "couche transport",
>   "cours d’eau",
>   "course à pied",
>   "couture",
>   "couvertures",
>   "couvre-chefs",
>   "créatures",
>   "cricket",
>   "crimes",
>   "crustacés",
>   "cryptographie",
>   "cuisine",
>   "cyclisme",
>   "céréales",
>   "dames",
>   "danse",
>   "danses",
>   "death metal",
>   "dentisterie",
>   "départements",
>   "dermatologie",
>   "desserts",
>   "dessin",
>   "dialectes",
>   "didactique",
>   "dindons",
>   "dinosaures",
>   "diplomatie",
>   "distinctions",
>   "drogues",
>   "droit",
>   "droit du travail",
>   "échafaudage",
>   "échecs",
>   "biogéographie",
>   "écologie",
>   "économie",
>   "édifices",
>   "édition",
>   "éducation",
>   "électricité",
>   "électronique",
>   "électrotechnique",
>   "éléments",
>   "élevage",
>   "embryologie",
>   "entomologie",
>   "enzymes",
>   "épices",
>   "équitation",
>   "escrime",
>   "états",
>   "ethnologie",
>   "ethnonymes",
>   "étoiles",
>   "famille",
>   "fantastique",
>   "fauconnerie",
>   "félins",
>   "ferroviaire",
>   "figures",
>   "finance",
>   "fiscalité",
>   "fleurs",
>   "fontainerie",
>   "football",
>   "foresterie",
>   "franc-maçonnerie",
>   "fromages",
>   "fruits",
>   "gâteaux",
>   "généalogie",
>   "génétique",
>   "genres musicaux",
>   "gentilés",
>   "géographie",
>   "géologie",
>   "géomatique",
>   "géométrie",
>   "géophysique",
>   "géostatistique",
>   "glaciologie",
>   "gladiateurs",
>   "golf",
>   "golfes",
>   "grades",
>   "grammaire",
>   "gravure",
>   "gymnastique",
>   "handball",
>   "héraldique",
>   "hindouisme",
>   "hippisme",
>   "hippologie",
>   "histoire",
>   "histologie",
>   "hockey",
>   "horlogerie",
>   "horticulture",
>   "hydraulique",
>   "ichtyologie",
>   "îles",
>   "illégalité",
>   "imprimerie",
>   "industrie",
>   "industrie pétrolière",
>   "infographie",
>   "informatique",
>   "insectes",
>   "instruments",
>   "instruments de mesure",
>   "insultes",
>   "intelligence artificielle",
>   "Internet",
>   "islam",
>   "jardinage",
>   "jazz",
>   "jeu de paume",
>   "jeux",
>   "jeux de cartes",
>   "jeux vidéo",
>   "jonglerie",
>   "jouets",
>   "journalisme",
>   "judaïsme",
>   "judo",
>   "justice",
>   "karaté",
>   "langage Java",
>   "langages",
>   "langues",
>   "législation",
>   "légumes",
>   "lexicographie",
>   "lézards",
>   "LGBT",
>   "lianes",
>   "linguistique",
>   "littérature",
>   "liturgie",
>   "livre",
>   "localités",
>   "logique",
>   "logistique",
>   "loisirs",
>   "lutherie",
>   "léporidé",
>   "machines",
>   "maçonnerie",
>   "magnétisme",
>   "mah-jong",
>   "maintenance",
>   "maladies",
>   "maladies de l’œil",
>   "mammifères",
>   "management",
>   "marbrerie",
>   "maréchalerie",
>   "marine",
>   "maroquinerie",
>   "marsupial",
>   "mathématiques",
>   "probabilités",
>   "mécanique",
>   "médecine",
>   "médecine non conv",
>   "médecine vétérinaire",
>   "média",
>   "médicaments",
>   "menuiserie",
>   "mercatique",
>   "métallurgie",
>   "métaplasmes",
>   "météorologie",
>   "scientifiques",
>   "métrologie",
>   "meubles",
>   "microbiologie",
>   "militaire",
>   "minéralogie",
>   "minéraux",
>   "miroiterie",
>   "mollusques",
>   "monarchie",
>   "monnaies",
>   "montagnes",
>   "motocyclisme",
>   "Moyen Âge",
>   "muscle",
>   "muscles",
>   "musculation",
>   "musiciens",
>   "musique",
>   "musiques",
>   "mycologie",
>   "mythologie",
>   "narratologie",
>   "natation",
>   "navigation",
>   "neurologie",
>   "noblesse",
>   "nosologie",
>   "novlangue",
>   "nucléaire",
>   "numismatique",
>   "nutrition",
>   "oenologie",
>   "oiseaux",
>   "optique",
>   "optométrie",
>   "ornement",
>   "ornithologie",
>   "un os",
>   "outils",
>   "paléographie",
>   "paléontologie",
>   "palmier",
>   "papeterie",
>   "papillons",
>   "pâtes alimentaires",
>   "patinage",
>   "pâtisserie",
>   "pays",
>   "pêche",
>   "pêches",
>   "pédologie",
>   "peinture",
>   "pelote",
>   "pétanque",
>   "pétrochimie",
>   "pharmacologie",
>   "philatélie",
>   "philosophie",
>   "phobies",
>   "phonétique",
>   "phonétique classique",
>   "photographie",
>   "physiologie",
>   "physique",
>   "planche à roulettes",
>   "plantes",
>   "plantes aromatiques",
>   "plomberie",
>   "plongée",
>   "poésie",
>   "points cardinaux",
>   "poires",
>   "poissons",
>   "poker",
>   "police",
>   "politique",
>   "pommes",
>   "ponctuations",
>   "positions",
>   "préhistoire",
>   "préparations",
>   "primates",
>   "programmation",
>   "programmation orientée objet",
>   "protéines",
>   "protocoles",
>   "proverbes",
>   "provinces",
>   "psychiatrie",
>   "psychologie",
>   "quartiers",
>   "régions",
>   "religieux",
>   "religion",
>   "religions",
>   "reliure",
>   "reproduction",
>   "reptiles",
>   "requins",
>   "réseaux informatiques",
>   "rhétorique",
>   "robotique",
>   "roches",
>   "rongeur",
>   "rugby",
>   "salades",
>   "saliculture",
>   "sandwichs",
>   "satellites",
>   "science-fiction",
>   "sciences",
>   "Scrabble",
>   "sculpture",
>   "seigneuries",
>   "sentiments",
>   "serpents",
>   "serrurerie",
>   "sexualité",
>   "sidérurgie",
>   "singes",
>   "ski alpin",
>   "ski de fond",
>   "snowboard",
>   "snowboard/Documentation",
>   "sociolinguistique",
>   "sociologie",
>   "soldats",
>   "sport",
>   "sport de combat",
>   "sport de glisse",
>   "sportifs",
>   "sports",
>   "sports de combat",
>   "sports de glisse",
>   "squelette",
>   "statistiques",
>   "stéréotomie",
>   "substances",
>   "surf",
>   "sylviculture",
>   "électoraux",
>   "taille de pierre",
>   "tauromachie",
>   "technique",
>   "technologie",
>   "télécommunications",
>   "téléinformatique",
>   "téléphonie",
>   "temps géologiques",
>   "tennis",
>   "textile",
>   "textiles",
>   "théâtre",
>   "théologie",
>   "théorie des graphes",
>   "thermodynamique",
>   "tonnellerie",
>   "topographie",
>   "topologie",
>   "toponymes",
>   "toponymie",
>   "tourisme",
>   "transport",
>   "travail",
>   "typographie",
>   "unités",
>   "symboles unités",
>   "urbanisme",
>   "usinage",
>   "ustensiles",
>   "vaudou",
>   "véhicules",
>   "vents",
>   "versification",
>   "vêtements",
>   "viandes",
>   "vins",
>   "virologie",
>   "virus",
>   "viticulture",
>   "vitrerie",
>   "voitures",
>   "wiki",
>   "yoga",
>   "zoologie"]

Regional labels, from https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_pays_du_Wiktionnaire
and https://fr.wiktionary.org/wiki/Catégorie:Modèles_régionaux_du_Wiktionnaire:

> regionLabels :: HashSet Text
> regionLabels = setFromList [
>   "Acadie",
>   "Afrique",
>   "Afrique du Sud",
>   "Algérie",
>   "Allemagne",
>   "Alsace",
>   "Amérique centrale",
>   "Amérique du Nord",
>   "Amérique du Sud",
>   "Amérique latine",
>   "Andorre",
>   "Anjou",
>   "Antilles",
>   "Aquitaine",
>   "Argentine",
>   "Australie",
>   "Autriche",
>   "Auvergne",
>   "Baléares",
>   "Belgique",
>   "Luxembourg",
>   "Belize",
>   "Bénin",
>   "Berry",
>   "Bolivie",
>   "Bordelais",
>   "Bourgogne",
>   "Bretagne",
>   "Brésil",
>   "Burkina Faso",
>   "Cameroun",
>   "Canada",
>   "Catalogne",
>   "Champagne",
>   "Chili",
>   "Chine",
>   "Colombie",
>   "Commonwealth",
>   "Congo",
>   "Congo-Brazzaville",
>   "Congo-Kinshasa",
>   "Corse",
>   "Corée du Nord",
>   "Corée du Sud",
>   "Costa Rica",
>   "Cuba",
>   "Côte d’Ivoire",
>   "Écosse",
>   "Espagne",
>   "Émirats arabes unis",
>   "États-Unis",
>   "Europe",
>   "France",
>   "Franche-Comté",
>   "Gascogne",
>   "Gaspésie",
>   "Guadeloupe",
>   "Guinée",
>   "Guyane",
>   "Haïti",
>   "Honduras",
>   "Île-de-France",
>   "Inde",
>   "Irlande",
>   "Jamaïque",
>   "Japon",
>   "Kirghizistan",
>   "Languedoc",
>   "Languedoc-Roussillon",
>   "Le Mans",
>   "Liban",
>   "Liechtenstein",
>   "Limousin",
>   "Lorraine",
>   "Louisiane",
>   "Lyonnais",
>   "Madagascar",
>   "Maghreb",
>   "Malaisie",
>   "Mali",
>   "Maroc",
>   "Marseille",
>   "Martinique",
>   "Maurice",
>   "Mayotte",
>   "Midi",
>   "Midi toulousain",
>   "Moldavie",
>   "Mongolie",
>   "Namibie",
>   "Nantes",
>   "Nicaragua",
>   "Niger",
>   "Nigeria",
>   "Nord-Pas-de-Calais",
>   "Normandie",
>   "Nouvelle-Calédonie",
>   "Nouvelle-Zélande",
>   "Occitanie",
>   "Océanie",
>   "Ouzbékistan",
>   "Panama",
>   "Paraguay",
>   "Paris",
>   "Pays-Bas",
>   "Pays basque",
>   "Picardie",
>   "Poitou",
>   "Polynésie française",
>   "Portugal",
>   "Provence",
>   "Pyrénées-Orientales",
>   "Pérou",
>   "Québec",
>   "Quercy",
>   "République dominicaine",
>   "Réunion",
>   "Rhône-Alpes",
>   "Roumanie",
>   "Royaume-Uni",
>   "République Démocratique du Congo",
>   "Salvador",
>   "Savoie",
>   "Suède",
>   "Suisse",
>   "Sénégal",
>   "Taïwan",
>   "Tchad",
>   "Terre-Neuve",
>   "Togo",
>   "Transnistrie",
>   "Tunisie",
>   "Uruguay",
>   "Var",
>   "Velay",
>   "Vendée",
>   "Venezuela",
>   "Viêt Nam"]

Other relevant labels from https://fr.wiktionary.org/wiki/Catégorie:Modèles_de_contexte:

> otherLabels :: HashSet Text
> otherLabels = setFromList [
>   "archaïque",
>   "argot",  -- see also the template rule that handles templates starting with "argot"
>   "désuet",
>   "vulgaire"]
>
> contextLabels = domainLabels <> regionLabels <> otherLabels


> handleContextTemplate :: Template -> AnnotatedText
> handleContextTemplate template =
>   annotate [mapFromList [("rel", "context"), ("language", "fr"), ("page", get "0" template)]] ""
>
> handleMiscContextTemplate :: Template -> AnnotatedText
> handleMiscContextTemplate template =
>   annotate [mapFromList [("rel", "context"), ("language", "fr"), ("page", get "1" template)]] ""


Evaluating templates
====================

Section headings
----------------

Handle the {{S}} template that starts many sections. If it has a second
argument, it's probably a part-of-speech template. Otherwise, it introduces a
section named for its first argument.

> handleSectionTemplate :: Template -> AnnotatedText
> handleSectionTemplate template =
>   case (lookup "2" template) of
>     Just lang -> annotate [mapFromList [("language", lang), ("pos", get "1" template), ("etym", findWithDefault "1" "num" template)]] "POS"
>     Nothing   -> annotate [] (get "1" template)


Language sections start with a {{langue}} template that usually just contains
the BCP 47 language code, which is what we want.

> handleLanguageCodeTemplate :: Template -> AnnotatedText
> handleLanguageCodeTemplate template = annotFromText (getLanguage "1" template)
>
> getLanguage :: Text -> Template -> Text
> getLanguage key map = fromLanguage $ lookupLanguage "fr" $ get key map

Links
-----

> handleLinkTemplate :: Template -> AnnotatedText
> handleLinkTemplate t = buildA $ do
>   put "language" (getLanguage "2" t)
>   adapt "page" arg1 t
>   adapt "sense" ["sens"] t
>   visible ["dif", "1"] t


Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedText
> handleTranslationTemplate t = buildA $ do
>   put "rel" "translation"
>   put "language" (getLanguage "1" t)
>   adapt "page" arg2 t
>   visible ["dif", "2"] t


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
>   | member x contextLabels     = handleContextTemplate
>   | isPrefixOf "argot" x       = handleContextTemplate
>   | otherwise                  = skipTemplate

