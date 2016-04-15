> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
> module Text.MediaWiki.Wiktionary.English where
> import WikiPrelude
> import Text.MediaWiki.Templates
> import Text.MediaWiki.AnnotatedText
> import Text.MediaWiki.SplitUtils
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.Sections
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Wiktionary.Base
> import qualified Data.Aeson as Ae
> import Data.Attoparsec.Text
> import Data.LanguageNames


Parsing sections
================

> enHandlePage :: Text -> Text -> [WiktionaryFact]
> enHandlePage title text =
>   let sections = parsePageIntoSections text in
>     concat (map (enParseSection title) sections)
>
> enHandleFile :: Text -> FilePath -> IO ()
> enHandleFile title filename = do
>   contents <- (readFile filename) :: IO Text
>   mapM_ (println . Ae.encode) (enHandlePage title contents)


Choosing an appropriate section parser
--------------------------------------

`enParseSection` takes in a title and a WikiSection structure, builds
a WiktionaryTerm structure for the term we're defining, and passes it on to
a function that will extract WiktionaryFacts.

> enParseSection :: Text -> WikiSection -> [WiktionaryFact]
> enParseSection title (WikiSection {headings=headings, content=content}) =
>   -- The first two headings are the meaningless level-1 heading and the
>   -- language heading. If those aren't there, bail out.
>   case uncons (drop 1 headings) of
>     Nothing -> []
>     -- langHeading will contain the level-2 heading (for the language),
>     -- and subheads will contain levels 3 and later. Now we need to look
>     -- through the subheads for details about the term we're parsing
>     -- and what kind of section to parse.
>     Just (langHeading, subheads) ->
>       let language    = lookupLanguage "en" langHeading
>           maybePos    = findPartOfSpeech subheads
>           etymNumber  = findEtymologyNumber subheads
>           sectionType = getSectionType subheads
>       in case maybePos of
>         -- If there's no heading we recognize as a part of speech, then
>         -- this is a section we don't want to parse. (This may change if
>         -- we decide to parse etymology sections.)
>         Nothing  -> []
>         Just pos ->
>           -- Build the WiktionaryTerm object and pass it on to the
>           -- section parser.
>           let thisTerm = WiktionaryTerm {
>             wtText=title,
>             wtLanguage=Just language,
>             wtEtym=Just etymNumber,
>             wtPos=Just pos,
>             wtSense=Nothing
>             } in chooseSectionParser sectionType thisTerm content

`chooseSectionParser` selects a particular function for making WiktionaryFacts,
based on the type of section we're parsing.

> chooseSectionParser :: Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> chooseSectionParser "POS" = parseDefinition
> chooseSectionParser "Translations" = parseTranslations
> chooseSectionParser "Synonyms" = parseRelation "synonym"
> chooseSectionParser "Antonyms" = parseRelation "antonym"
> chooseSectionParser "Hyponyms" = parseRelation "hyponym"
> chooseSectionParser "Hypernyms" = parseRelation "hypernym"
> chooseSectionParser "Meronyms" = parseRelation "meronym"
> chooseSectionParser "Holonyms" = parseRelation "holonym"
> chooseSectionParser "Troponyms" = parseRelation "troponym"
> chooseSectionParser "Coordinate terms" = parseRelation "coordinate"
> chooseSectionParser "Derived terms" = parseRelation "derived"
> chooseSectionParser "Related terms" = parseRelation "related"
> chooseSectionParser "See also" = parseRelation "related"
> chooseSectionParser x = const (const [])


The part-of-speech/definition section
-------------------------------------

The section that's labeled something like "Noun" contains definitions of the
given term in English, as a numbered list.

Here, we parse the Wikitext for the numbered list, then pass its entries
on to `definitionToFacts`. If there's a parse error, we return nothing for
this section.

> parseDefinition :: WiktionaryTerm -> Text -> [WiktionaryFact]
> parseDefinition thisTerm text =
>   let defs = parseOrDefault [] pDefinitionSection text in
>     concat (map (definitionToFacts "en" thisTerm) defs)

Skip miscellaneous lines at the start of the section: try to parse each line as
pDefinitionList, and if that fails, parse one line, throw it out, and
recursively run this parser to parse the rest.

> pDefinitionSection :: Parser [LabeledDef]
> pDefinitionSection =
>   pDefinitionList <|>
>   (newLine >> pDefinitionSection) <|>
>   (wikiTextLine ignoreTemplates >> newLine >> pDefinitionSection)
>
> pDefinitionList :: Parser [LabeledDef]
> pDefinitionList = extractNumberedDefs <$> orderedList enTemplates "#"


The translation section
-----------------------

> parseTranslations :: WiktionaryTerm -> Text -> [WiktionaryFact]
> parseTranslations thisTerm text = parseOrDefault [] (pTranslationSection thisTerm) text
>
> pTranslationSection :: WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationSection thisTerm = concat <$> many1 (pTranslationGroup thisTerm)
>
> pTranslationGroup :: WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationGroup thisTerm = do
>   optionalTextChoices [newLine]
>   maybeSense <- pTranslationTopTemplate
>   let senseTerm = thisTerm {wtSense=maybeSense}
>   items <- concat <$> many1 pTranslationColumn
>   optionalTextChoices [newLine]
>   return (map (annotationToFact "en" senseTerm) (filter translationsOnly items))
>
> translationsOnly :: Annotation -> Bool
> translationsOnly annot = (get "rel" annot) == "translation"

The `pTranslationTopTemplate` rule parses the template that starts a
translation section, which may or may not be labeled with a word sense. It
returns a Maybe Text that contains the word sense if present.

> pTranslationTopTemplate :: Parser (Maybe Text)
> pTranslationTopTemplate = pTransTop <|> pCheckTransTop
>
> pTransTop = do
>   template <- specificTemplate enTemplates "trans-top"
>   newLine
>   return (lookup "1" template)
> pCheckTransTop = do
>   specificTemplate enTemplates "checktrans-top"
>   newLine
>   return Nothing

A column of translations (yes, this is purely a layout thing) ends with either
{{trans-mid}}, which separates columns, or {{trans-bottom}}, which ends the section.
The translations themselves are contained in a bulleted list.

> pTranslationColumn :: Parser [Annotation]
> pTranslationColumn = concat <$> many1 (pTranslationItem <|> pTranslationBlankLine) <* pTranslationColumnEnd
> pTranslationColumnEnd = specificTemplate enTemplates "trans-mid" <|> specificTemplate enTemplates "trans-bottom" <* many1 newLine

The procedure for getting translations out of a bunch of bullet points involved a few
chained procedures, which of course occur from right to left:

  - Parse a bullet-pointed list entry.

  - Find the items the bullet-pointed list entry contains. There may be
    multiple of them, because some translation entries are nested lists --
    multiple kinds of translations for the same language, for example.
    `extractTopLevel` turns these items into a flat list.

  - `extractTopLevel` gave us a list of AnnotatedTexts. We want just their
    annotations, representing everything we want to know about the
    translations, in one big list. So we `A.concat` all the AnnotatedTexts
    together, and then take the combined list of annotations from that.

> pTranslationItem :: Parser [Annotation]
> pTranslationItem = getAnnotations <$> mconcat <$> extractTopLevel <$> listItem enTemplates "*"
>
> pTranslationBlankLine :: Parser [Annotation]
> pTranslationBlankLine = newLine >> return []


Relation sections
-----------------

> parseRelation :: Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseRelation rel thisTerm text = parseOrDefault [] (pRelationSection rel thisTerm) text
>
> pRelationSection :: Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pRelationSection rel thisTerm = map (assignRel rel)
>                                 <$> concat
>                                 <$> map (entryToFacts "en" thisTerm)
>                                 <$> extractTextLines
>                                 <$> bulletList enTemplates "*"


Finding headings
================

The following list of parts of speech comes from:
https://en.wiktionary.org/wiki/Wiktionary:Entry_layout#Part_of_speech

We skip entries in the sub-lists called 'Symbols and characters', 'Han
characters and language-specific varieties', and 'Lojban-specific parts of
speech'.

> partsOfSpeech :: HashSet Text
> partsOfSpeech = setFromList [
>   "Adjective", "Adverb", "Ambiposition", "Article", "Circumposition",
>   "Classifier", "Conjunction", "Contraction", "Counter", "Determiner",
>   "Interjection", "Noun", "Numeral", "Participle", "Particle",
>   "Postposition", "Preposition", "Pronoun", "Proper noun", "Verb",
>   "Circumfix", "Combining form", "Infix", "Interfix", "Prefix", "Root",
>   "Suffix", "Phrase", "Proverb", "Prepositional phrase", "Acronym",
>   "Symbol"
>   ]
>
> findPartOfSpeech :: [Text] -> Maybe Text
> findPartOfSpeech = findHeading partsOfSpeech
>
> findEtymologyNumber :: [Text] -> Text
> findEtymologyNumber headings =
>   case findPrefixedHeading "Etymology " headings of
>     Just x -> x
>     Nothing -> "1"

Generalizing the type of a heading:

> getSectionType :: [Text] -> Text
> getSectionType headings =
>   case (lastMay headings) of
>     Nothing -> "Language"
>     Just heading ->
>       if elem heading partsOfSpeech
>         then "POS"
>         else if isPrefixOf "Etymology " heading
>           then "Etymology"
>           else heading


Evaluating templates
====================

Labels
------

The `{{label}}` template applies labels to a definition, specifying its
grammar, its semantic context, or possibly other things. It involves a lot of
Lua code for special cases.

The following labels are basically just part of the syntax of an entry, and
we'd like to skip them. We also include the empty string here, corresponding
to template arguments that aren't there.

> syntacticLabels :: HashSet Text
> syntacticLabels = setFromList [
>   "", "and", "&", "or", "_", "now", "except", "except in", "outside",
>   "especially", "chiefly", "mainly", "mostly", "particularly", "primarily",
>   "excluding", "extremely", "frequently", "including", "literally",
>   "literal", "many", "markedly", "mildly", "now", "nowadays", "of", "of a",
>   "of an", "often", "originally", "possibly", "rarely", "slightly",
>   "sometimes", "somewhat", "strongly", "typically", "usually", "very"]

These labels provide grammatical (not semantic) information. We'll keep them
separate in case we ever want to output them:

> grammarLabels :: HashSet Text
> grammarLabels = setFromList [ "abbreviation", "acronym", "active", "active voice",
>   "in the active", "ambitransitive", "archaic-verb-form", "attributive",
>   "attributively", "auxiliary", "by ellipsis", "by extension", "causative",
>   "collectively", "comparable", "copulative", "copular", "countable",
>   "ditransitive", "emphatic", "ergative", "fractional", "idiomatic",
>   "idiom", "impersonal", "in the singular", "in singular", "in the dual",
>   "in dual", "in the plural", "in plural",
>   "in the mediopassive", "in mediopassive", "mediopassive", "inanimate",
>   "indefinite", "indef", "initialism", "intransitive", "not comparable",
>   "notcomp", "uncomparable", "middle", "middle voice", "in the middle",
>   "onomatopoeia", "ordinal", "plural", "passive", "passive voice",
>   "in the passive", "plural only", "pluralonly", "plurale tantum",
>   "possessive pronoun", "postpositive", "productive", "reciprocal",
>   "reflexive", "set phrase", "singular", "singular only", "singulare tantum",
>   "no plural", "transitive", "uncountable", "usually plural",
>   "usually in the plural", "usually in plural", "mass noun", "a mass noun"]

Combine these together into a set of all labels we want to ignore.

> ignoredLabels :: HashSet Text
> ignoredLabels = syntacticLabels <> grammarLabels

> handleLabelTemplate :: Template -> AnnotatedText
> handleLabelTemplate template =
>   let entries     = map (\arg -> get arg template) ["2", "3", "4", "5"]
>       goodEntries = filter (\val -> not (elem val ignoredLabels)) entries
>       annotations = map labelToAnnotation goodEntries
>   in annotate annotations (asText "")
>
> labelToAnnotation :: Text -> Annotation
> labelToAnnotation label = mapFromList [("rel", "context"), ("language", "en"), ("page", label)]

Qualifiers are similar to labels, but take just a single argument.

> handleQualifierTemplate :: Template -> AnnotatedText
> handleQualifierTemplate template =
>   let label = get "1" template in
>     annotate [labelToAnnotation label] ""

Sense IDs
---------
Hooray! Wiktionary is finally starting to mark word senses with stable IDs
instead of just numbered lists. But we need to be able to extract that
information, mapping the sense number to the sense.

We'll make a "senseID" annotation that we pass on to the definition parser in
Wiktionary.Base.

> handleSenseIDTemplate :: Template -> AnnotatedText
> handleSenseIDTemplate template = singleAnnotation "senseID" (get "2" template)
>
> handleSenseTemplate :: Template -> AnnotatedText
> handleSenseTemplate template = singleAnnotation "senseID" (get "1" template)


Links
-----

> handleLinkTemplate :: Template -> AnnotatedText
> handleLinkTemplate t = buildA $ do
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   adapt "gloss" ["4", "gloss"] t
>   adapt "pos" ["pos"] t
>   visible ["3", "2"] t

Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedText
> handleTranslationTemplate t = buildA $ do
>   put "rel" "translation"
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   visible ["alt", "2"] t


Form-of templates
-----------------

> handleAbstractFormTemplate :: Template -> AnnotatedText
> handleAbstractFormTemplate t = buildA $ do
>   put "rel" ("form/" <> (get "1" t))
>   adapt "form" arg1 t
>   adapt "page" arg2 t
>   adapt "language" ["lang"] t
>   invisible
>
> handleFormTemplate :: Text -> Template -> AnnotatedText
> handleFormTemplate form t = buildA $ do
>   put "rel" ("form/" <> form)
>   put "form" form
>   adapt "page" arg1 t
>   adapt "language" ["lang"] t
>   invisible
>
> handleInflectionTemplate :: Template -> AnnotatedText
> handleInflectionTemplate t =
>   let forms = getAll ["3","4","5","6","7","8","9"] t
>       formStr = intercalate "+" forms
>   in buildA $ do
>       put "rel" ("form/" <> formStr)
>       adapt "language" ["lang"] t
>       adapt "page" arg1 t
>       invisible
>
> handleSpecificFormsTemplate :: Language -> [Text] -> Template -> AnnotatedText
> handleSpecificFormsTemplate language forms t =
>   let annotations = [annotationFromList 
>                       [("rel", "form/" <> form),
>                        ("language", fromLanguage language),
>                        ("page", pageName (get "1" t))] | form <- forms]
>   in annotate annotations ""
>
> handleSpanishVerbTemplate :: Template -> AnnotatedText
> handleSpanishVerbTemplate t =
>   let forms = getAll ["pers", "person", "num", "number", "tense", "mood", "gen", "gender"] t
>       formal = case (get "formal" t) of
>                  "y"   -> ["formal"]
>                  "yes" -> ["formal"]
>                  "n"   -> ["informal"]
>                  "no"  -> ["informal"]
>                  _     -> []
>       formStr = intercalate "+" (formal <> forms)
>   in buildA $ do
>       put "rel" ("form/" <> formStr)
>       put "language" "es"
>       adapt "page" ["1", "inf", "verb", "infinitive"] t
>       invisible
>
> handleSpanishCompoundTemplate t =
>   let forms = getAll ["mood", "person"] t
>       formStr = intercalate "+" ("compound":forms)
>   in buildA $ do
>       put "rel" ("form/" <> formStr)
>       put "language" "es"
>       adapt "page" ["3", "1"] t
>       invisible

There are many templates named "xx-verb form of", and they tend to work in
similar ways: several arguments to the template are symbols for various kinds
of inflections, and we just want to join these symbols together. We just need
to know which language it is, and which template arguments to look up.

> handleLanguageFormTemplate :: Language -> [Text] -> Template -> AnnotatedText
> handleLanguageFormTemplate language formKeys t =
>   let forms = getAll formKeys t
>       formStr = intercalate "+" forms
>   in buildA $ do
>     put "rel" ("form/" <> formStr)
>     put "language" (fromLanguage language)
>     adapt "page" arg1 t
>     invisible

We need to handle:

- [[Category:Form-of_templates]]
- feminine plural past participle of
- feminine singular past participle of
- masculine plural past participle of
- masculine singular past participle of
- neuter singular past participle of
- gerund of
- imperative of
- reflexive of
- verbal noun of


Putting it all together
-----------------------

> enTemplates :: TemplateProc
> enTemplates "l"         = handleLinkTemplate
> enTemplates "link"      = handleLinkTemplate
> enTemplates "m"         = handleLinkTemplate
> enTemplates "mention"   = handleLinkTemplate
> enTemplates "label"     = handleLabelTemplate
> enTemplates "lbl"       = handleLabelTemplate
> enTemplates "lb"        = handleLabelTemplate
> enTemplates "qualifier" = handleQualifierTemplate
> enTemplates "sense"     = handleSenseTemplate
> enTemplates "senseid"   = handleSenseIDTemplate
> enTemplates "t"         = handleTranslationTemplate
> enTemplates "t+"        = handleTranslationTemplate
> enTemplates "t-"        = handleTranslationTemplate
> -- "t\195\184" is "tø" written as a bytestring
> enTemplates "t\195\184" = handleTranslationTemplate
> -- ignore the more uncertain translation templates, t-check and t+check
>
> enTemplates "form of"             = handleAbstractFormTemplate
> enTemplates "alternative form of" = handleFormTemplate "alternate"
> enTemplates "alternate form of"   = handleFormTemplate "alternate"
> enTemplates "alt form of"         = handleFormTemplate "alternate"
> enTemplates "alt form"            = handleFormTemplate "alternate"
> enTemplates "altform"             = handleFormTemplate "alternate"
> enTemplates "inflection of"       = handleInflectionTemplate
> enTemplates "conjugation of"      = handleInflectionTemplate
>
> -- Should these be handled in more detail than just extracting their text?
> enTemplates "initialism of"       = useArg "1"
> enTemplates "acronym of"          = useArg "1"
> enTemplates "synonym of"          = useArg "1"
> enTemplates "w"                   = useArg "1"
>
> enTemplates "en-simple past of"                    = handleSpecificFormsTemplate "en" ["past"]
> enTemplates "en-past of"                           = handleSpecificFormsTemplate "en" ["past", "past+ptcp"]
> enTemplates "past of"                              = handleFormTemplate "past"
> enTemplates "past tense of"                        = handleFormTemplate "past"
> enTemplates "past participle of"                   = handleFormTemplate "past+ptcp"
> enTemplates "past active participle of"            = handleFormTemplate "past+actv+ptcp"
> enTemplates "past passive participle of"           = handleFormTemplate "past+pasv+ptcp"
> enTemplates "present tense of"                     = handleFormTemplate "pres"
> enTemplates "present participle of"                = handleFormTemplate "pres+ptcp"
> enTemplates "present active participle of"         = handleFormTemplate "pres+actv+ptcp"
> enTemplates "present passive participle of"        = handleFormTemplate "pres+pasv+ptcp"
> enTemplates "future participle of"                 = handleFormTemplate "fut+ptcp"
> enTemplates "participle of"                        = handleFormTemplate "ptcp"
> enTemplates "active participle of"                 = handleFormTemplate "ptcp+actv"
> enTemplates "passive participle of"                = handleFormTemplate "ptcp+pasv"
> enTemplates "en-third-person singular of"          = handleSpecificFormsTemplate "en" ["3+s+pres"]
> enTemplates "en-third person singular of"          = handleSpecificFormsTemplate "en" ["3+s+pres"]
> enTemplates "en-archaic second-person singular of" = handleSpecificFormsTemplate "en" ["archaic+2+s+pres"]
> enTemplates "en-archaic third-person singular of"  = handleSpecificFormsTemplate "en" ["archaic+3+s+pres"]
> enTemplates "second-person singular past of"       = handleFormTemplate "archaic+2+s+past"
>
> enTemplates "plural of"                   = handleFormTemplate "p"
> enTemplates "gerund of"                   = handleFormTemplate "ger"
>
> enTemplates "en-irregular plural of"      = handleSpecificFormsTemplate "en" ["p"]
> enTemplates "en-comparative of"           = handleSpecificFormsTemplate "en" ["comp"]
> enTemplates "en-superlative of"           = handleSpecificFormsTemplate "en" ["sup"]
>
> enTemplates "de-inflected form of" = handleSpecificFormsTemplate "de" ["?"]
> enTemplates "de-zu-infinitive of"  = handleSpecificFormsTemplate "de" ["zu"]
> enTemplates "de-verb form of"      = handleLanguageFormTemplate "de" ["2","3","4","5"]
>
> enTemplates "es-verb form of"      = handleSpanishVerbTemplate
> enTemplates "es-compound of"       = handleSpanishCompoundTemplate
>
> enTemplates "pt-noun form of" = handleLanguageFormTemplate "pt" ["2","3","4"]
> enTemplates "pt-adj form of"  = handleLanguageFormTemplate "pt" ["2","3","4"]
> enTemplates "pt-verb form of" = handleLanguageFormTemplate "pt" ["3","4","5","6"]
>
> enTemplates _         = skipTemplate

