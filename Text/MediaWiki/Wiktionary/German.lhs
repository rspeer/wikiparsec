> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Export only the top-level, namespaced functions.

> module Text.MediaWiki.Wiktionary.German
>   (deParseWiktionary, deTemplates) where
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
> deParseCombinedSection term content = parseOrDefault (error $ cs content) (pCombinedSection term) content

> pCombinedSection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pCombinedSection term = mconcat <$> many (pSubsection term)

> pSubsection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pSubsection term =
>   (pDefinitionSubsection term) <|>
>   (pRelationSubsection "Unterbegriffe" "hyponym" term) <|>
>   (pRelationSubsection "Oberbegriffe" "hypernym" term) <|>
>   (pRelationSubsection "Gegenwörter" "antonym" term) <|>
>   (pRelationSubsection "Sinnverwandte Wörter" "related" term) <|>
>   (pRelationSubsection "Synonyme" "synonym" term) <|>
>   (pRelationSubsection "Wortbildungen" "derived" term) <|>
>   (pRelationSubsection "Verkleinerungsformen" "hasForm/diminutive" term) <|>
>   (pRelationSubsection "Vergrößerungsformen" "hasForm/augmentative" term) <|>
>   (pRelationSubsection "Weibliche Wortformen" "hasForm/feminine" term) <|>
>   (pRelationSubsection "Männliche Wortformen" "hasForm/masculine" term) <|>
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
>   parseOrDefault (error $ cs text) (pDeTranslationSection thisTerm) text
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
> handleTranslationTemplate t = buildA $ do
>   put "rel" "translation"
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   visible ["3", "2"] t


Part of speech headings
-----------------------

> handlePOSTemplate :: Template -> AnnotatedText
> handlePOSTemplate t = buildA $ do
>   adapt "pos" arg1 t
>   put "language" (getLanguage "2" t)
>   return "POS"
>
> getLanguage :: Text -> Template -> Text
> getLanguage key map = fromLanguage $ lookupLanguage "de" $ get key map

Inflected forms
---------------

> handleFormTemplate = skipTemplate


Putting it all together
-----------------------

> deTemplates :: TemplateProc
> deTemplates "Wortart" = handlePOSTemplate
> deTemplates "Ü"       = handleTranslationTemplate
> deTemplates "Ü?"      = handleTranslationTemplate
> deTemplates "Üt"      = handleTranslationTemplate
> deTemplates "Üxx4"    = handleTranslationTemplate
> deTemplates "Üxx4?"   = handleTranslationTemplate

Cases that need to be checked with expressions instead of plain pattern
matches:

> deTemplates x
>   | isInfixOf " Übersicht" x   = handleFormTemplate
>   | otherwise                  = skipTemplate

