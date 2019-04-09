> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Export only the top-level, namespaced functions.

> module Text.MediaWiki.Wiktionary.German
>   (deParseWiktionary, deTemplates) where
> import WikiPrelude
> import Text.MediaWiki.Templates
> import Text.MediaWiki.AnnotatedText
> import Text.SplitUtils
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

> deParseWiktionary :: Text -> Text -> [WiktionaryFact]
> deParseWiktionary title text =
>   let sections = parsePageIntoSections text in
>     concat (map (deParseSection title) sections)


Finding headings
================

Unlike the English Wiktionary, the German Wiktionary frequently uses templates
within headings. `evalHeading` will read a heading as WikiText, returning an
AnnotatedText object.

TODO: unify this with French?

> evalHeading :: Text -> AnnotatedText
> evalHeading heading =
>   case parseOnly (annotatedWikiText deTemplates) heading of
>     Left err      -> error err
>     Right atext   -> atext

The `partOfSpeechMap` maps German terms for parts of speech (as they appear in
templates) to the simple set of POS symbols used in WordNet or ConceptNet.

> partOfSpeechMap :: Text -> Text
> partOfSpeechMap "Substantiv" = "n"
> partOfSpeechMap "Eigenname"  = "n"
> partOfSpeechMap "Toponym"    = "n"
> partOfSpeechMap "Nachname"   = "n"
> partOfSpeechMap "Vorname"    = "n"
> partOfSpeechMap "Adjektiv"   = "a"
> partOfSpeechMap "Adverb"     = "r"
> partOfSpeechMap "Verb"       = "v"
> partOfSpeechMap _            = "_"


Parsing sections
================

`deParseSection` takes in a title and a WikiSection structure, builds
a WiktionaryTerm structure for the term we're defining, and passes it on to
a function that will extract WiktionaryFacts.

> deParseSection :: Text -> WikiSection -> [WiktionaryFact]
> deParseSection title (WikiSection {headings=headings, content=content}) =
>   case headings of
>     (_:_:pos:trans:[]) ->
>       let term = getHeadingTerm title (evalHeading pos) in
>         if trans == "Übersetzungen"
>           then (deParseTranslations term content)
>           else []
>     (_:_:pos:[]) ->
>       let term = getHeadingTerm title (evalHeading pos) in
>         deParseCombinedSection term content
>     _ -> []

> getHeadingTerm :: Text -> AnnotatedText -> WiktionaryTerm
> getHeadingTerm title annot =
>   annotationToTerm "de" (insertMap "page" title (mconcat (getAnnotations annot)))

Parsing the big section
-----------------------

The "Wortart" (part-of-speech) section contains sub-sections defining the word
in various ways.

> deParseCombinedSection :: WiktionaryTerm -> Text -> [WiktionaryFact]
> deParseCombinedSection term content = parseOrDefault [] (pCombinedSection term) content

> pCombinedSection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pCombinedSection term = mconcat <$> many (pSubsection term)

> pSubsection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pSubsection term =
>   (pDefinitionSubsection term) <|>
>   (pRelationSubsection "Unterbegriffe" "*hypernym" term) <|>
>   (pRelationSubsection "Oberbegriffe" "hypernym" term) <|>
>   (pRelationSubsection "Gegenwörter" "distinct" term) <|>
>   (pRelationSubsection "Sinnverwandte Wörter" "related" term) <|>
>   (pRelationSubsection "Synonyme" "synonym" term) <|>
>   (pRelationSubsection "Wortbildungen" "derived" term) <|>
>   (pRelationSubsection "Verkleinerungsformen" "*form/diminutive" term) <|>
>   (pRelationSubsection "Vergrößerungsformen" "*form/augmentative" term) <|>
>   (pRelationSubsection "Weibliche Wortformen" "*form/feminine" term) <|>
>   (pRelationSubsection "Männliche Wortformen" "*form/masculine" term) <|>
>   (pFactsFromTemplate term) <|>
>   -- If we don't match any of our desired section types, consume a line
>   -- and return nothing
>   (newLine >> return []) <|>
>   pArbitraryLine term
>
> pArbitraryLine :: WiktionaryTerm -> Parser [WiktionaryFact]
> pArbitraryLine thisTerm = do
>   line <- templateText ignoreTemplates <|> wikiTextLine ignoreTemplates
>   newLine
>   return []
>

Defining quasi-section parsers
------------------------------

> pFactsFromTemplate :: WiktionaryTerm -> Parser [WiktionaryFact]
> pFactsFromTemplate thisTerm =
>   map (annotationToFact "de" thisTerm)
>     <$> getAnnotations
>     <$> templateValue deTemplates

> pDefinitionSubsection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pDefinitionSubsection term = do
>   specificTemplate ignoreTemplates "Bedeutungen"
>   many1 newLine
>   defs <- pLabeledDefinitionList deTemplates
>   return (mconcat (map (definitionToFacts "de" term) defs))

> pRelationSubsection :: Text -> Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pRelationSubsection heading rel term = do
>   specificTemplate ignoreTemplates heading
>   many1 newLine
>   pDeRelationSection rel term

> pDeRelationSection :: Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pDeRelationSection rel thisTerm =
>   map (assignRel rel)
>     <$> mconcat
>     <$> map (entryToFacts "de" thisTerm)
>     <$> extractLabeledItems
>     <$> indentedList deTemplates ":"

Parsing the translations section
--------------------------------

There's a function in Wiktionary.Base called `parseTranslations` that applies
to multiple languages, but the German translation section is too much different
-- the word-sense information comes from inside the definitions, not from
a table heading. So let's define it from scratch here.

> deParseTranslations :: WiktionaryTerm -> Text -> [WiktionaryFact]
> deParseTranslations thisTerm text =
>   parseOrDefault [] (pDeTranslationSection thisTerm) text
>
> pDeTranslationSection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pDeTranslationSection thisTerm = do
>   skipSpace
>   string "{{Ü-Tabelle"
>   items <- many ((newLine >> return [])
>                  <|> (pSectionBoundary >> return [])
>                  <|> (template ignoreTemplates >> return [])
>                  <|> pDeTranslationLine thisTerm
>                  <|> (wikiTextLine ignoreTemplates >> newLine >> return []))
>   return (mconcat items)

pSectionBoundary parses the argument names in the huge template that makes
up the translation section.

> pSectionBoundary :: Parser ()
> pSectionBoundary = string "|" >> templateArgName >> skipSpace

> pDeTranslationLine :: WiktionaryTerm -> Parser [WiktionaryFact]
> pDeTranslationLine thisTerm = do
>   textWith "*"
>   annotatedWikiTextWithout ":" deTemplates
>   string ":"
>   skipSpace
>   mconcat <$> sepBy (pDeTranslationItem thisTerm) "; "
>
> pDeTranslationItem :: WiktionaryTerm -> Parser [WiktionaryFact]
> pDeTranslationItem thisTerm = do
>   labels <- pBracketedLabels
>   atext <- annotatedWikiTextWithout ";" deTemplates
>   let labelAppliers = map applyLabel labels
>   let labeledAnnotations = labelAppliers <*> (filter isTranslation (getAnnotations atext))
>   return (map (annotationToFact "de" thisTerm) labeledAnnotations)
>
> applyLabel :: Text -> Annotation -> Annotation
> applyLabel label anno = insertMap "senseID" ("def." <> label) anno


Evaluating templates
====================

Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedText
> handleTranslationTemplate t = skipEmpty $ annotationBuilder $ do
>   put "rel" "translation"
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   visible ["3", "2"] t
>
> skipEmpty :: AnnotatedText -> AnnotatedText
> skipEmpty atext =
>   if (getText atext == "")
>     then mempty
>     else atext

Part of speech headings
-----------------------

> handlePOSTemplate :: Template -> AnnotatedText
> handlePOSTemplate t = annotationBuilder $ do
>   put "pos" (partOfSpeechMap (get "1" t))
>   put "language" (getLanguage "2" t)
>   return "POS"
>
> getLanguage :: Text -> Template -> Text
> getLanguage key map = fromLanguage $ lookupLanguage "de" $ get key map

Inflected forms
---------------

Here's an example of a German inflection template:

    {{Deutsch Verb Übersicht
    |Präsens_ich=—
    |Präsens_du=—
    |Präsens_er, sie, es=regnet
    |Präteritum_ich=regnete
    |Konjunktiv II_ich=regnete
    |Imperativ Singular=—
    |Imperativ Plural=—
    |Partizip II=geregnet
    |Hilfsverb=haben
    |unpersönlich=ja
    }}

Recognize these templates by their name. Generally, any template with
Übersicht in its name is a table of inflections. We need to exclude
Basque inflections here because those tables work entirely differently.

> isInflectionTemplate :: Text -> Bool
> isInflectionTemplate name =
>   (isInfixOf "Übersicht" name) && (not (isPrefixOf "Baskisch" name))

Most of the arguments describe different forms of the word. We should skip
values that are empty or —, skip keys that aren't forms, and skip positional
arguments.

> skippedInflectionArgs :: HashSet Text
> skippedInflectionArgs = setFromList [
>   "weitere_konjugationen", "weitere_deklinationen",
>   "hilfsverb", "unpersönlich", "klasse",
>   "wort", "bedeutung", "herkunft", "gegenwörter",
>   "unicode", "radikal", "zusatzstriche", "strichzahl", "viereckenindex",
>   "cangjie", "n", "sg", "pl", "endung", "syn", "s", "k",
>   "morse", "braille", "html", "html-hex", "html-dez", "block", "språk",
>   "html-entity", "hilfsverb2",
>   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
>   ]

We also want to skip all arguments starting with "Bild", which are used for
adding pictures to the entry, and arguments that are written phonetically in
IPA.

> skippedInflectionPrefixes :: [Text]
> skippedInflectionPrefixes = ["bild", "ipa_", "stamm", "navi", "1", "2",
>                              "deklination", "klasse"]

Some values, as well, indicate that we shouldn't use this entry as a form. Some
of these, like [?], indicate that someone just put a meaningless value in a
template.  Others are incomplete and depend on a human reader's intuition, such
as listing the plural of "la leche entera" as simply "las".

> skippedInflectionValues :: HashSet Text
> skippedInflectionValues = setFromList [
>   "ja", "nein", "--", "[?]", "——", "...",
>   "le", "la", "los", "las", "&mdash;", "&ndash;",
>   "die", "der", "das", "mehr"
>   ]

`handleInflectionTemplate` parses an inflection template, taking the above
data into account.

> handleInflectionTemplate :: Template -> AnnotatedText
> handleInflectionTemplate t =
>   let pairs = (filter keepInflectionArg) $ (map normalizeInflectionArg) $ mapToList t
>       (languageName, _) = splitFirst " " (get "0" t)
>       language = lookupLanguage "de" languageName
>       annotations = map (makeInflectionAnnotation language) pairs
>   in annotate annotations ""
>
> normalizeInflectionName :: Text -> Text
> normalizeInflectionName text =
>   toLower $ replace " " "_" $ dropWhileEnd (== '*') text
>
> normalizeInflectionArg :: (Text, Text) -> (Text, Text)
> normalizeInflectionArg (name, value) =
>   (normalizeInflectionName name, value)
>
> keepInflectionArg :: (Text, Text) -> Bool
> keepInflectionArg (name, value) =
>   not (member name skippedInflectionArgs) &&
>   not (any (\prefix -> isPrefixOf prefix name) skippedInflectionPrefixes) &&
>   (length value) >= 0 && not (isPrefixOf "Flexion:" value) &&
>   not (member value skippedInflectionValues)
>
> makeInflectionAnnotation :: Language -> (Text, Text) -> Annotation
> makeInflectionAnnotation language (name, value) = mapFromList [
>   ("rel", "*form/" <> name),
>   ("language", fromLanguage language),
>   ("form", name),
>   ("page", value)]


Putting it all together
-----------------------

> deTemplates :: TemplateProc
> deTemplates "Wortart" = handlePOSTemplate
> deTemplates "Ü"       = handleTranslationTemplate
> deTemplates "Ü?"      = handleTranslationTemplate
> deTemplates "Üt"      = handleTranslationTemplate
> deTemplates "Üxx4"    = handleTranslationTemplate
> deTemplates "Üxx4?"   = handleTranslationTemplate
> deTemplates "Arab"    = useArg "1"

Cases that need to be checked with expressions instead of plain pattern
matches:

> deTemplates x
>   | isInflectionTemplate x     = handleInflectionTemplate
>   | otherwise                  = skipTemplate

