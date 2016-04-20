> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

This file defines how to parse Wiktionary entries, as a layer above the basic
handling of wiki syntax in `Wikitext.lhs`.

> module Text.MediaWiki.Wiktionary.Base where
> import WikiPrelude
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.SplitUtils
> import Text.MediaWiki.AnnotatedText
> import Text.MediaWiki.Templates
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator
> import Data.Aeson (ToJSON, toJSON, (.=), encode, object)
> import Data.LanguageNames
> import Data.LanguageType
> import Text.Language.Normalize (normalizeText)
> import Text.MediaWiki.HTML (extractWikiTextFromHTML)
> import Text.Show.Unicode

Handling entire pages
---------------------

> handleFile :: (Text -> Text -> [WiktionaryFact]) -> Text -> FilePath -> IO ()
> handleFile languageParser title filename = do
>   contents <- (readFile filename) :: IO ByteString
>   let fromHTML = extractWikiTextFromHTML contents
>   mapM_ (println . show) (languageParser title fromHTML)
>
> handleFileJSON :: (Text -> Text -> [WiktionaryFact]) -> Text -> FilePath -> IO ()
> handleFileJSON languageParser title filename = do
>   contents <- (readFile filename) :: IO ByteString
>   let fromHTML = extractWikiTextFromHTML contents
>   mapM_ (println . encode) (languageParser title fromHTML)

Data types
----------

A WiktionaryTerm is a piece of text that can be defined on Wiktionary. It is
defined by its term text, which is required, along with other fields which may
be unknown or missing: the language it's in, a label for its word sense, its
part of speech, and its etymology label (which is like a much broader word
sense).

(Wiktionary entries for etymologically-unrelated homographs separate them into
sections named, for example, "Etymology 1" and "Etymology 2". In these cases,
"1" and "2" are the etymology labels. Most words only have a single etymology,
and by default their etymology label is "1".)

> data WiktionaryTerm = WiktionaryTerm {
>   wtText :: Text,
>   wtLanguage :: Maybe Language,
>   wtSense :: Maybe Text,
>   wtPos :: Maybe Text,
>   wtEtym :: Maybe Text
> } deriving (Eq)

We export a term to JSON by constructing an object that keeps the values that
are `Just val`, and excludes the ones that are `Nothing`. We also use this
JSON representation as the string representation of a WiktionaryTerm.

> instance ToJSON WiktionaryTerm where
>   toJSON term =
>     let maybePairs = [("text", Just (wtText term)),
>                       ("language", fromLanguage <$> (wtLanguage term)),
>                       ("pos", wtPos term),
>                       ("etym", wtEtym term),
>                       ("sense", wtSense term)]
>         existingPairs = mapMaybe moveSecondMaybe maybePairs
>     in object [key .= value | (key, value) <- existingPairs]
>
> instance Show WiktionaryTerm where
>   show term =
>     let items = [Just (wtText term),
>                  fromLanguage <$> (wtLanguage term),
>                  wtPos term,
>                  wtEtym term,
>                  wtSense term]
>         showList = ushow (squishMaybes items)
>     in cs ("(term " <> showList <> ")")
>
> squishMaybes :: [Maybe Text] -> [Text]
> squishMaybes list = reverse $ map (fromMaybe "") $ listDropWhile isNothing $ reverse list
>
> moveSecondMaybe :: (a, Maybe b) -> Maybe (a, b)
> moveSecondMaybe (first, Just second) = Just (first, second)
> moveSecondMaybe (first, Nothing)     = Nothing
>
> term :: [Text] -> WiktionaryTerm
> term items =
>   let language = toLanguage (fromMaybe "und" (index items 1)) in
>     WiktionaryTerm {
>       wtText = normalizeText language (fromMaybe (error "term is empty") (index items 0)),
>       wtLanguage = toLanguage <$> (index items 1),
>       wtPos = nonEmpty (index items 2),
>       wtEtym = nonEmpty (index items 3),
>       wtSense = nonEmpty (index items 4)
>       }
>
> simpleTerm :: Language -> Text -> WiktionaryTerm
> simpleTerm language text = term [text, fromLanguage language]
>
> termPos :: Language -> Text -> Text -> WiktionaryTerm
> termPos language text pos = term [text, fromLanguage language, pos]
>
> termSense :: Language -> Text -> Text -> Text -> Text -> WiktionaryTerm
> termSense language text pos etym sense = term [text, fromLanguage language, pos, etym, sense]
>

A WiktionaryFact expresses a relationship between terms that we can extract
from a page.

> data WiktionaryFact = WiktionaryFact Text WiktionaryTerm WiktionaryTerm deriving (Show, Eq)
>
> instance ToJSON WiktionaryFact where
>   toJSON (WiktionaryFact rel from to) = object ["rel" .= rel, "from" .= from, "to" .= to]
>
> makeFact :: Text -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryFact
> makeFact = WiktionaryFact
> makeGenericFact = makeFact "RelatedTo"
>
> assignRel :: Text -> WiktionaryFact -> WiktionaryFact
> assignRel rel (WiktionaryFact _ from to) = WiktionaryFact rel from to


Annotations
-----------

Working with annotations:

> linkableAnnotation :: Annotation -> Bool
> linkableAnnotation annot = (get "page" annot /= "") && (get "namespace" annot == "")
>
> linkableAnnotations :: AnnotatedText -> [Annotation]
> linkableAnnotations atext = filter linkableAnnotation (getAnnotations atext)
>
> plainLinkAnnotation :: Annotation -> Bool
> plainLinkAnnotation annot = linkableAnnotation annot && (findWithDefault "link" "rel" annot) == "link"
>
> plainLinkAnnotations :: AnnotatedText -> [Annotation]
> plainLinkAnnotations astring = filter plainLinkAnnotation (getAnnotations astring)

Converting an Annotation representing a term to a WiktionaryTerm:

(TODO: explain why languages get complicated here)

> annotationToTerm :: Language -> Annotation -> WiktionaryTerm
> annotationToTerm thisLang annot =
>   let maybeLanguage = (annotationLanguage thisLang annot) in
>     WiktionaryTerm {
>       wtText=(normalizeText (fromMaybe "und" maybeLanguage) (pageName (get "page" annot))),
>       wtLanguage=maybeLanguage,
>       wtPos=(lookup "pos" annot),
>       wtEtym=(lookup "etym" annot),
>       wtSense=(lookup "sense" annot)
>     }
>
> pageName :: Text -> Text
> pageName name = fst (splitFirst "#" name)
>
> annotationLanguage :: Language -> Annotation -> Maybe Language
> annotationLanguage thisLang annot =
>   case (lookup "language" annot) of
>     Just language -> Just (toLanguage language)
>     Nothing ->
>       case (lookup "section" annot) of
>         Just section -> sectionLanguage thisLang section
>         Nothing -> Nothing
>
> sectionLanguage :: Language -> Text -> Maybe Language
> sectionLanguage thisLang sectionRef =
>   case uncons sectionRef of
>     Just ('#', language) -> Just (lookupLanguage thisLang language)
>     otherwise            -> Nothing
>
> annotationToFact :: Language -> WiktionaryTerm -> Annotation -> WiktionaryFact
> annotationToFact language thisTerm annot =
>   let otherTerm = annotationToTerm language annot
>       rel       = findWithDefault "link" "rel" annot
>   in makeFact rel thisTerm otherTerm

We might have an annotation assigning a sense ID to this text:

> findSenseID :: AnnotatedText -> Maybe Text
> findSenseID atext = findSenseIDInList (getAnnotations atext)
>
> findSenseIDInList :: [Annotation] -> Maybe Text
> findSenseIDInList (annot:rest) =
>   case (lookup "senseID" annot) of
>     Just x -> Just x
>     Nothing -> findSenseIDInList rest
> findSenseIDInList [] = Nothing


Definition sections
-------------------

Definition sections in many languages of Wiktionary take the form of a numbered
list. For example, the English dictionary has sections labeled with a part of
speech, such as "Noun", whose contents are a numbered list of English definitions
of the word.

Here, we parse the Wikitext for the numbered list, then pass its entries
on to `definitionToFacts`. If there's a parse error, we return nothing for
this section.

> parseDefinitions :: Language -> TemplateProc -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseDefinitions language tproc thisTerm text =
>   let defs = parseOrDefault [] (pDefinitionSection tproc) text in
>     concat (map (definitionToFacts language thisTerm) defs)

Skip miscellaneous lines at the start of the section: try to parse each line as
pDefinitionList, and if that fails, parse one line, throw it out, and
recursively run this parser to parse the rest.

> pDefinitionSection :: TemplateProc -> Parser [LabeledDef]
> pDefinitionSection tproc =
>   pDefinitionList tproc <|>
>   (newLine >> pDefinitionSection tproc) <|>
>   (wikiTextLine ignoreTemplates >> newLine >> pDefinitionSection tproc)
>
> pDefinitionList :: TemplateProc -> Parser [LabeledDef]
> pDefinitionList tproc = extractNumberedDefs <$> orderedList tproc "#"

To distinguish different definitions, we associate them with an automatically
generated ID such as "def.1.1".

TODO give an example because this is all confusing

> type LabeledDef = (Text, AnnotatedText)
>
> extractNumberedDefs = extractNumbered "def"
>
> extractNumbered :: Text -> ListItem -> [LabeledDef]
> extractNumbered prefix (OrderedList items) = extractNumberedIter prefix 1 items
>
> extractNumberedIter :: Text -> Int -> [ListItem] -> [LabeledDef]
> extractNumberedIter prefix counter list =
>   let newPrefix = mconcat [prefix, ".", cs (show counter)]
>   in case list of
>     ((Item item):rest)         -> (newPrefix, item):(extractNumberedIter prefix (counter + 1) rest)
>     ((OrderedList items):rest) -> (extractNumberedIter newPrefix 1 items)
>                                   ++ (extractNumberedIter prefix (counter + 1) rest)
>     _:rest                     -> extractNumberedIter prefix counter rest
>     []                         -> []

Converting definitions to facts:

> definitionToFacts :: Language -> WiktionaryTerm -> LabeledDef -> [WiktionaryFact]
> definitionToFacts language thisTerm defPair =
>   let defText = snd defPair
>       -- get a sense either from the SenseID annotation, or failing that,
>       -- from the label that comes with the definition
>       defSense = mplus (findSenseID defText) (Just (fst defPair))
>       termSense = thisTerm {wtSense=defSense}
>       defPieces = splitDefinition (stripSpaces (getText defText))
>   in (map (makeDefinitionFact termSense language) defPieces)
>      <> (map (annotationToFact language termSense) (linkableAnnotations defText))

> makeDefinitionFact termSense language definition =
>   makeFact "definition" termSense (simpleTerm language definition)
>
> splitDefinition :: Text -> [Text]
> splitDefinition definition =
>   if definition == "" then []
>   else
>     case parseOnly pDefinitionText definition of
>       Right results -> results
>       Left err -> error err


Parsing the language of definitions
-----------------------------------

> pDefinitionText :: Parser [Text]
> pDefinitionText = (pDefCommas <|> pDefSemicolons <|> pDefAnything)
>
> pCommaItem     = textWithout " ,;:."
> pSemicolonItem = textWithout ";."
>
> pDefCommas :: Parser [Text]
> pDefCommas = sepBy1 pCommaItem (string ", ") <* endOfInput
>
> pDefSemicolons :: Parser [Text]
> pDefSemicolons = do
>   items <- sepBy pSemicolonItem (string "; ")
>   option '.' (char '.')
>   endOfInput
>   return items
>
> pDefAnything :: Parser [Text]
> pDefAnything = do
>   text <- takeWhile (const True)
>   return [text]


Relation sections
-----------------

> parseRelation :: Language -> TemplateProc -> Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseRelation language tproc rel thisTerm text =
>   parseOrDefault [] (pRelationSection language tproc rel thisTerm) text
>
> pRelationSection :: Language -> TemplateProc -> Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pRelationSection language tproc rel thisTerm =
>   map (assignRel rel)
>     <$> concat
>     <$> map (entryToFacts language thisTerm)
>     <$> extractTextLines
>     <$> bulletList tproc "*"
>
> entryToFacts :: Language -> WiktionaryTerm -> AnnotatedText -> [WiktionaryFact]
> entryToFacts language thisTerm defText =
>   let defSense  = findSenseID defText
>       termSense = thisTerm {wtSense=defSense}
>   in map (annotationToFact language termSense) (plainLinkAnnotations defText)



The translation section
-----------------------

Most of the work involved in parsing translation sections is fundamentally
the same between different languages. However, details are different between
languages, most notably the names of the templates involved.

If we passed in all these details as arguments to `parseTranslations`, the type
of `parseTranslations` and its helper functions would be horrifying. Instead,
we'll group them together into a TranslationSectionInfo struct.

> data TranslationSectionInfo = TranslationSectionInfo {
>   tsLanguage :: Language,             -- the language to be parsed
>   tsTemplateProc :: TemplateProc,     -- the template procedure to use
>   tsStartRule :: Parser (Maybe Text), -- a parser for the start of a translation list
>   tsIgnoreRule :: Parser (),          -- a parser for lines to ignore
>   tsEndRule :: Parser ()              -- a parser for the end of a translation list
> }
>
>
> parseTranslations :: TranslationSectionInfo -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseTranslations tsInfo thisTerm text =
>   parseOrDefault [] (pTranslationSection tsInfo thisTerm) text
>
> pTranslationSection :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationSection tsInfo thisTerm = concat <$> many1 (pTranslationGroup tsInfo thisTerm)

A translation group is a portion of the "Translations" section that all applies to the same
word sense. It's delimited by a start template and an end template. The start template might
return a word sense, or might return Nothing.

> pTranslationGroup :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationGroup tsInfo thisTerm = do
>   optionalTextChoices [newLine]
>   maybeSense <- tsStartRule tsInfo
>   let senseTerm = thisTerm {wtSense=maybeSense}
>   let language = tsLanguage tsInfo
>   items <- pTranslationGroupBody tsInfo senseTerm
>   tsEndRule tsInfo
>   optionalTextChoices [newLine]
>   return items

> pTranslationGroupBody :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationGroupBody tsInfo thisTerm =
>   mconcat <$>
>     map (extractTranslations (tsLanguage tsInfo) thisTerm) <$>
>     mconcat <$>
>     many1 (pTranslationItem tsInfo <|> (tsIgnoreRule tsInfo >> return []))
>
> extractTranslations :: Language -> WiktionaryTerm -> AnnotatedText -> [WiktionaryFact]
> extractTranslations language thisTerm atext =
>   let translationAnnotations = filter isTranslation (getAnnotations atext) in
>     map (annotationToFact language thisTerm) translationAnnotations
>
> isTranslation :: Annotation -> Bool
> isTranslation a = lookup "rel" a == Just "translation"

The procedure for getting translations out of a bunch of bullet points involved a few
chained procedures, which of course occur from right to left:

To get translation candidates out of a bunch of bullet points, we need to find
the items the bullet-pointed list entry contains.

There may be multiple of them, because some translation entries are nested
lists -- multiple kinds of translations for the same language, for example.
`extractTopLevel` turns these items into a flat list.

> pTranslationItem :: TranslationSectionInfo -> Parser [AnnotatedText]
> pTranslationItem tsInfo = extractTopLevel <$> listItem (tsTemplateProc tsInfo) "*"

This rule will generally be useful in writing the "ignore" parse rule:

> pBlankLine :: Parser ()
> pBlankLine = newLine >> return ()


Looking up sections
-------------------

> findHeading :: HashSet Text -> [Text] -> Maybe Text
> findHeading choices headings =
>   let filtered = filter (\x -> elem x choices) headings
>   in headMay filtered
>
> findPrefixedHeading :: Text -> [Text] -> Maybe Text
> findPrefixedHeading prefix headings =
>   let filtered = filter (isPrefixOf prefix) headings
>       mapped   = map (drop (length prefix)) filtered
>   in headMay mapped
>
> intersectLists :: (Eq a) => [a] -> [a] -> [a]
> intersectLists list1 list2 = filter (\x -> elem x list1) list2


Transforming templates
----------------------

Many of the template functions we define will involve converting a Template
value into an AnnotatedText. To get some convenient `do` syntax for this, we
use `put`, a function defined in `WikiPrelude` that uses a Writer monad.

Repeatedly running `put` will build up a Map, and in the end it returns a
value. Here, the value it returns will be the Text, and the Map will become
an Annotation on that text.

> buildA :: Writer Annotation Text -> AnnotatedText
> buildA m =
>   let (text, anno) = runWriter m in
>     annotate [anno] text
>
> adapt :: Text -> [Text] -> Template -> Writer Annotation Text
> adapt keyTarget keySources = (put keyTarget) . (getPrioritized keySources)
>
> visible :: [Text] -> Template -> Writer Annotation Text
> visible keySources = return . (getPrioritized keySources)
>
> invisible :: Writer Annotation Text
> invisible = return ""
>
> arg1, arg2, arg3 :: [Text]
> arg1 = ["1"]
> arg2 = ["2"]
> arg3 = ["3"]
