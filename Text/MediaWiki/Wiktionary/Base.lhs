> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

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
>     in cs ("(term " ⊕ showList ⊕ ")")
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

`makeFact` or one of its derived functions should be used to create
WiktionaryFacts.

Sometimes we extract data that goes in the opposite direction form the relation
we want: for example, instead of discovering which word `from` is a form of,
we discover a word that is a form of it. In this case, we output a relation
that's prefixed with an asterisk, telling `makeFact` to strip off the asterisk
and switch the arguments.

> makeFact :: Text -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryFact
> makeFact rel from to =
>   case (uncons rel) of
>     Just ('*', rev) -> WiktionaryFact rev to from
>     _               -> WiktionaryFact rel from to
>
> makeGenericFact = makeFact "RelatedTo"
>
> assignRel :: Text -> WiktionaryFact -> WiktionaryFact
> assignRel rel fact@(WiktionaryFact oldRel from to) =
>   case oldRel of
>     "link" -> makeFact rel from to
>     _      -> fact


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
> plainLinkAnnotations atext = filter plainLinkAnnotation (getAnnotations atext)
>
> languageTaggedAnnotation :: Annotation -> Bool
> languageTaggedAnnotation annot = plainLinkAnnotation annot && (findWithDefault "" "language" annot) /= ""
> languageTaggedAnnotations atext = filter languageTaggedAnnotation (getAnnotations atext)


Converting an Annotation representing a term to a WiktionaryTerm:

(TODO: explain why languages get complicated here)

> annotationToFact :: Language -> WiktionaryTerm -> Annotation -> WiktionaryFact
> annotationToFact thisLang thisTerm annot =
>   let otherTerm = annotationToTerm thisLang annot
>       -- The annotation might come with a term sense (term senses can sneak
>       -- in from many different directions). If it does, it specifies a
>       -- sense of the word *being defined*, not the word it's connected to.
>       termSense = case (lookup "senseID" annot) of
>                     Nothing -> thisTerm
>                     Just sense -> thisTerm {wtSense=Just sense}
>       rel       = findWithDefault "link" "rel" annot
>   in makeFact rel termSense otherTerm

It may seem intuitive that, if the Annotation doesn't come with a language, we
would default to using `thisLang`.

That would actually introduce errors. If a word is being defined in English,
that does not necessarily mean that any word linked in the definition is an
English word. It could be the same as the word being defined, instead.

As an example, on the English Wiktionary, a definition of the Spanish word
"tengo" is "First-person singular ([[yo]]) present indicative form of
[[tener]]." Neither "yo" or "tener" here should be considered an English word,
despite that they appear in an English definition. If we have no way to
determine the language of a link, we should leave it unspecified to be inferred
later.

The reason we take in `thisLang` is because we might have to look up a language
that's given as a *section* name, such as [[tener#Spanish]]. In this case, we
don't get a language code, we get the name of the language in `thisLang` and we
need to convert it to a language code, and that's what we need `thisLang` for.

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

Working with lists of headings:

> getTextInList :: Int -> [AnnotatedText] -> Maybe Text
> getTextInList idx atexts = getText <$> index atexts idx
>
> getAnnotationInList :: Int -> Text -> [AnnotatedText] -> Maybe Text
> getAnnotationInList idx key atexts =
>   -- use the Maybe monad to return Nothing for any missing index
>   index atexts idx >>= \atext -> lookup key (mconcat (getAnnotations atext))

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
>   pNumberedDefinitionList tproc <|>
>   (newLine >> pDefinitionSection tproc) <|>
>   (wikiTextLine ignoreTemplates >> newLine >> pDefinitionSection tproc)
>
> pNumberedDefinitionList :: TemplateProc -> Parser [LabeledDef]
> pNumberedDefinitionList tproc = extractNumberedDefs <$> orderedList tproc "#"
>
> pLabeledDefinitionList :: TemplateProc -> Parser [LabeledDef]
> pLabeledDefinitionList tproc = extractLabeledDefs <$> indentedList tproc ":"

To distinguish different definitions, we associate them with an automatically
generated ID such as "def.1.1".

TODO give an example because this is all confusing

> type LabeledDef = (Text, AnnotatedText)
>
> extractNumberedDefs = extractNumbered "def"
>
> extractNumbered :: Text -> ListItem -> [LabeledDef]
> extractNumbered prefix (OrderedList items) = extractNumberedIter prefix 1 items
> extractNumbered prefix _ = error "Wrong type of list"
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

In German and some other Wiktionaries that have followed its lead, lists have
specifically numbered word senses instead of automatic numbering. Instead of
starting with `#`, for example, a definition line starts with `:[1]`.

We aren't going to parse sub-definitions, the ones that start with `::[a]` or
something to make them part of the definition above.

> extractLabeledDefs :: ListItem -> [LabeledDef]
> extractLabeledDefs (IndentedList items) = mconcat (map extractLabeledDefItem items)
> extractLabeledDefs _ = error "Wrong type of list"
>
> extractLabeledDefItem :: ListItem -> [LabeledDef]
> extractLabeledDefItem (Item item) =
>   case parseOnly pLabeledItem (getText item) of
>     Left err -> []
>     Right (maybeLabelList, defn) ->
>       case maybeLabelList of
>         Just labelList -> [adjustLabel label defn item | label <- labelList]
>         Nothing        -> [("", item)]
> extractLabeledDefItem _ = []
>
> adjustLabel :: Text -> Text -> AnnotatedText -> LabeledDef
> adjustLabel senseLabel defn atext =
>   let senseID     = "def." ⊕ senseLabel
>       annotations = (singletonMap "senseID" senseID):getAnnotations atext
>       atext'      = annotate annotations defn
>   in (senseID, atext')
>
> extractLabeledItems :: ListItem -> [AnnotatedText]
> extractLabeledItems items = map snd (extractLabeledDefs items)

Items in definition lists, and their corresponding entries in sections that
describe particular relations, can have complex lists of label numbers. For
example, the entry that translates the German word "gehen" into English looks
like:

    [1] {{Ü|en|walk}}; [1–3, 7, 13] {{Ü|en|go}}; ...

Here, sense 1 of "gehen" is translated as "walk", but all of senses 1, 2, 3, 7,
and 13 are translated as "go".

> pLabeledItem :: Parser (Maybe [Text], Text)
> pLabeledItem = do
>   labels <- pMaybeBracketedLabels
>   text <- takeText
>   return (labels, text)
>
> pMaybeBracketedLabels :: Parser (Maybe [Text])
> pMaybeBracketedLabels = pBracketedLabels <|> return Nothing
>
> pBracketedLabels = do
>   string "["
>   labels <- pLabels
>   string "]"
>   skipSpace
>   return (Just labels)
>
> pLabels :: Parser [Text]
> pLabels = mconcat <$> sepBy1 pCommaSeparatedLabel (char ',' >> skipSpace)
>
> pCommaSeparatedLabel :: Parser [Text]
> pCommaSeparatedLabel = pLabelRange <|> (pure <$> pSingleLabel)
>
> pSingleLabel = textWith "0123456789abcdefghij"
>
> pLabelRange = do
>   startNum <- decimal
>   textWith "-–—"
>   endNum <- decimal
>   return (map tshow [startNum..endNum])

Converting definitions to facts:

> definitionToFacts :: Language -> WiktionaryTerm -> LabeledDef -> [WiktionaryFact]
> definitionToFacts language thisTerm defPair =
>   let defText = snd defPair
>       -- get a sense either from the SenseID annotation, or failing that,
>       -- from the label that comes with the definition
>       defSense = mplus (findSenseID defText) (nonEmpty (Just (fst defPair)))
>       termSense = thisTerm {wtSense=defSense}
>       defPieces = splitDefinition (stripSpaces (getText defText))
>   in (map (makeDefinitionFact termSense language) defPieces)
>      ⊕ (map (annotationToFact language termSense) (linkableAnnotations defText))

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
>   text <- takeText
>   return [text]


Relation sections
-----------------

Similarly to the translation section, we group together some parameters that
vary by language in RelationSectionInfo.

> data RelationSectionInfo = RelationSectionInfo {
>   rsLanguage :: Language,             -- the language to be parsed
>   rsTemplateProc :: TemplateProc,     -- the template procedure to use
>   rsItemRule :: (TemplateProc -> Parser [AnnotatedText])  -- how to parse items
> }
>
> parseRelation :: RelationSectionInfo -> Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseRelation rsInfo rel thisTerm text =
>   parseOrDefault [] (pRelationSection rsInfo rel thisTerm) text
>
> pRelationSection :: RelationSectionInfo -> Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pRelationSection rsInfo rel thisTerm =
>   let tproc = (rsTemplateProc rsInfo)
>       language = rsLanguage rsInfo
>       itemRule = rsItemRule rsInfo
>   in map (assignRel rel)
>     <$> mconcat
>     <$> map (entryToFacts language thisTerm)
>     <$> mconcat
>     <$> many ((itemRule tproc) <|> pRelationIgnored)

pRelationItem is a sensible default function to pass as `rsItemRule`.

> pRelationItem :: TemplateProc -> Parser [AnnotatedText]
> pRelationItem tproc =
>   extractTopLevel <$> listItem tproc "*"
>
> pRelationIgnored :: Parser [AnnotatedText]
> pRelationIgnored = wikiTextLine ignoreTemplates >> newLine >> return []
>
> entryToFacts :: Language -> WiktionaryTerm -> AnnotatedText -> [WiktionaryFact]
> entryToFacts thisLang thisTerm defText =
>   let defSense  = findSenseID defText
>       termSense = thisTerm {wtSense=defSense}
>       annots    = plainLinkAnnotations defText
>   in map (annotationToFact thisLang termSense) annots


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
>   let filtered = filter (∈ choices) headings
>   in headMay filtered
>
> findPrefixedHeading :: Text -> [Text] -> Maybe Text
> findPrefixedHeading prefix headings =
>   let filtered = filter (isPrefixOf prefix) headings
>       mapped   = map (drop (length prefix)) filtered
>   in headMay mapped
>
> intersectLists :: (Eq a) => [a] -> [a] -> [a]
> intersectLists list1 list2 = filter (∈ list1) list2


Transforming templates
----------------------

Many of the template functions we define will involve converting a Template
value into an AnnotatedText. To get some convenient `do` syntax for this, we
use `put`, which lets us concatenate together values using a monad called
Writer.

The Writer monad maintains a state, which is a monoid, and the action of the monad
is to append things onto that monoid. I promise this will be really useful.

> put :: (IsMap map, Monoid (MapValue map), Eq (MapValue map)) => ContainerKey map -> MapValue map -> Writer map (MapValue map)
> put key value =
>   if (value == ø)
>     then writer (value, ø)
>     else writer (value, singletonMap key value)


Repeatedly running `put` will build up a Map, and in the end it returns a
value. Here, the value it returns will be the Text, and the Map will become
an Annotation on that text.

TODO: explain the rest of this

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
