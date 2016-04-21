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
>       if trans == "Übersetzungen"
>         then (deParseTranslations term content)
>         else []
>     (_:_:pos:[]) ->
>       let term = getHeadingTerm title (evalHeading pos) in
>         deParseCombinedSection term content
>     _ -> []

> getHeadingTerm :: AnnotatedText -> WiktionaryTerm
> getHeadingTerm title annot =
>   annotationToTerm "de" (insert "page" title (mconcat (getAnnotations annot)))

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
>   (pRelationSubsection "Verkleinerungsformen" "form/diminutive" term) <|>
>   (pRelationSubsection "Vergrößerungsformen" "form/augmentative" term) <|>
>   (pRelationSubsection "Weibliche Wortformen" "form/feminine" term) <|>
>   (pRelationSubsection "Männliche Wortformen" "form/masculine" term) <|>
>   -- If we don't match any of our desired section types, consume a line
>   -- and return nothing
>   (wikiTextLine ignoreTemplates >> newLine >> return [])
>   

Defining quasi-section parsers
------------------------------

> pDefinitionSubsection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pDefinitionSubsection term = do
>   specificTemplate ignoreTemplates "Bedeutungen"
>   defs <- pLabeledDefinitionList deTemplates
>   return (map (definitionToFacts "de" term) defs)

> pRelationSubsection :: Text -> Text -> WiktionaryTerm -> [WiktionaryFact]
> pRelationSubsection heading rel term = do
>   specificTemplate ignoreTemplates heading
>   pRelationSection "de" deTemplates rel term

> deParseDefinitions = parseDefinitions "de" deTemplates
> deParseRelation = parseRelation "de" deTemplates
> deParseTranslations = parseTranslations $ TranslationSectionInfo {
>   tsLanguage="de",
>   tsTemplateProc=deTemplates,
>   tsStartRule=pTransTop,
>   tsIgnoreRule=pTransMid <|> pBlankLine,
>   tsEndRule=pTransBottom
> }

The `pTranslationTopTemplate` rule parses the template that starts a
translation section. If this template has an argument, it labels the word
sense.

> pTransTop :: Parser (Maybe Text)
> pTransTop = string "{{Ü-Tabelle|Ü-links=\n" >> return Nothing
>
> pTransMid = string "|Ü-rechts=\n" >> return ()
>
> pTransBottom :: Parser ()
> pTransBottom = string "}}" >> return ()


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
>   | isinfixOf " Übersicht" x   = handleFormTemplate
>   | otherwise                  = skipTemplate

