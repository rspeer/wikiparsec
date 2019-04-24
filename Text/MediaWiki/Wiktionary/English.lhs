`Text.MediaWiki.Wiktionary.English`: parsing Wiktionary in English
==================================================================

This file defines specific rules for parsing the English Wiktionary, building
on the rules in `Text.MediaWiki.Wiktionary.Base`.

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Export only the top-level, namespaced functions.

> module Text.MediaWiki.Wiktionary.English
>   (enParseWiktionary, enTemplates, enParseRelation, enParseEtymology) where
> import WikiPrelude
> import Text.MediaWiki.Templates
> import Text.MediaWiki.AnnotatedText
> import Text.SplitUtils
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.Sections
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Wiktionary.Base
> import Data.Attoparsec.Text
> import Data.LanguageNames


Parsing entire pages
--------------------

This function can be passed as an argument to `handleFileJSON` in
Text.MediaWiki.Wiktionary.Base.

> enParseWiktionary :: Text -> Text -> [WiktionaryFact]
> enParseWiktionary title text =
>   let sections = parsePageIntoSections text in
>     concat (map (enParseSection title) sections)


Choosing an appropriate section parser
--------------------------------------

`enParseSection` takes in a title and a WikiSection structure (from
`Text.MediaWiki.Sections`), builds a WiktionaryTerm structure for the term
we're defining, and passes it on to a function that will extract
WiktionaryFacts.

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
>           thisTerm    = WiktionaryTerm {
>                           wtText=title,
>                           wtLanguage=Just language,
>                           wtEtym=Just etymNumber,
>                           wtPos=partOfSpeechMap <$> maybePos,
>                           wtSense=Nothing
>                           }
>       in chooseSectionParser sectionType thisTerm content

`chooseSectionParser` selects a particular function for making WiktionaryFacts,
based on the type of section we're parsing.

When we specify what kind of relation to extract, "hypernym" means that the
terms we find will be hypernyms of the word being defined, while "*hypernym"
reverses that -- the word being defined will be the hypernym of the words we
find.

Later, the `makeFact` function will see the asterisk and swap the arguments
when constructing a WiktionaryFact.

> chooseSectionParser :: Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> chooseSectionParser "POS" = enParseDefinitions
> chooseSectionParser "Translations" = enParseTranslations
> chooseSectionParser "Etymology" = enParseEtymology
> chooseSectionParser "Synonyms" = enParseRelation "synonym"
> chooseSectionParser "Antonyms" = enParseRelation "antonym"
> chooseSectionParser "Hyponyms" = enParseRelation "*hypernym"
> chooseSectionParser "Hypernyms" = enParseRelation "hypernym"
> chooseSectionParser "Meronyms" = enParseRelation "*holonym"
> chooseSectionParser "Holonyms" = enParseRelation "holonym"
> chooseSectionParser "Troponyms" = enParseRelation "troponym"
> chooseSectionParser "Coordinate terms" = enParseRelation "coordinate"
> chooseSectionParser "Derived terms" = enParseRelation "derived"
> chooseSectionParser "Related terms" = enParseRelation "related"
> chooseSectionParser "See also" = enParseRelation "related"

The default case, for a section type we don't know how to parse, is a function
that ignores two arguments and returns the empty list.

> chooseSectionParser x = const (const [])


Defining section parsers
------------------------

Defining section parsers by filling in the missing details in more general
functions, defined in `Text.MediaWiki.Wiktionary.Base`:

> enParseDefinitions = parseDefinitions "en" enTemplates
>
> enParseRelation = parseRelation $ RelationSectionInfo {
>   rsLanguage="en",
>   rsTemplateProc=enTemplates,
>   rsItemRule=pRelationItem
> }
>
> enParseTranslations = parseTranslations $ TranslationSectionInfo {
>   tsLanguage="en",
>   tsTemplateProc=enTemplates,
>   tsStartRule=(pTransTop <|> pCheckTransTop),
>   tsIgnoreRule=(pTransMid <|> pBlankLine),
>   tsEndRule=pTransBottom
> }

The `pTranslationTopTemplate` rule parses the template that starts a
translation section, which may or may not be labeled with a word sense. It
returns a Maybe Text that contains the word sense if present.

> pTransTop :: Parser (Maybe Text)
> pTransTop = do
>   template <- specificTemplate enTemplates "trans-top"
>   newLine
>   return (lookup "1" template)
>
> pCheckTransTop :: Parser (Maybe Text)
> pCheckTransTop = do
>   specificTemplate enTemplates "checktrans-top"
>   newLine
>   return Nothing
>
> pTransMid :: Parser ()
> pTransMid = specificTemplate enTemplates "trans-mid" >> return ()
>
> pTransBottom :: Parser ()
> pTransBottom = specificTemplate enTemplates "trans-bottom" >> return ()

In an etymology section, we just want to extract all the templates that
link to another word. We parse the section using `sectionAnnotated`, extract
the links, and create "related/etym" relations from them.

> enParseEtymology :: WiktionaryTerm -> Text -> [WiktionaryFact]
> enParseEtymology thisTerm text =
>   let etymParsed = parseOrDefault mempty (sectionAnnotated enTemplates) text
>       annots = languageTaggedAnnotations etymParsed
>       facts = map (annotationToFact "en" thisTerm) annots
>   in map (assignRel "related/etym") facts

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
> partOfSpeechMap :: Text -> Text
> partOfSpeechMap "Adjective"   = "a"
> partOfSpeechMap "Adverb"      = "r"
> partOfSpeechMap "Noun"        = "n"
> partOfSpeechMap "Pronoun"     = "n"
> partOfSpeechMap "Proper noun" = "n"
> partOfSpeechMap "Verb"        = "v"
> partOfSpeechMap _             = "_"
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
>   "", "and", "&", "or", "_", ",", "now", "except", "except in", "etc.",
>   "outside", "especially", "chiefly", "mainly", "mostly", "particularly",
>   "primarily", "excluding", "extremely", "frequently", "including",
>   "literally", "literal", "many", "markedly", "mildly", "now", "nowadays",
>   "of", "of a", "of an", "often", "originally", "possibly", "rarely",
>   "slightly", "sometimes", "somewhat", "strongly", "typically", "usually",
>   "very", "in", "of"]

Often we can tell by the prefix of a label that it describes a feature of the
word as a word, not of its meaning. This can be called the "use-mention
distinction". We would like to filter out labels that are about mentioning the
word and not about using it.

Some entries contain labels that look like `~ par`. They come out exactly this
way in the entry. I don't even know what it's supposed to mean, so let's leave
out these labels, too.

> mentionPrefixes :: [Text]
> mentionPrefixes = [
>   "~ ", "with ", "of ", "as ", "especially", "in ", "+ ", "by ",
>   "followed by ", "no longer", "often ", "chiefly ", "takes ",
>   "usually ", "on "
>   ]

These labels provide grammatical (not semantic) information. We'll keep them
separate in case we ever want to output them:

> grammarLabels :: HashSet Text
> grammarLabels = setFromList [
>   "abbreviation", "acronym", "active", "active voice",
>   "in the active", "ambitransitive", "archaic-verb-form", "attributive",
>   "attributively", "auxiliary", "by ellipsis", "by extension", "causative",
>   "collectively", "comparable", "copulative", "copular", "countable",
>   "ditransitive", "emphatic", "ergative", "fractional", "idiomatic",
>   "idiom", "impersonal", "in the singular", "in singular", "in the dual",
>   "in dual", "in the plural", "in plural",
>   "in the mediopassive", "in mediopassive", "mediopassive", "inanimate",
>   "indefinite", "indef", "initialism", "intransitive",
>   "mass noun", "a mass noun", "not comparable",
>   "notcomp", "uncomparable", "middle", "middle voice", "in the middle",
>   "onomatopoeia", "ordinal", "plural", "passive", "passive voice",
>   "in the passive", "plural only", "pluralonly", "plurale tantum",
>   "possessive", "possessive pronoun", "postpositive", "productive", "reciprocal",
>   "reflexive", "set phrase", "singular", "singular only", "singulare tantum",
>   "no plural", "transitive", "uds.", "uncountable", "usually plural",
>   "usually in the plural", "usually in plural"]

> usageLabels :: HashSet Text
> usageLabels = setFromList [
>   "obsolete", "rare", "dated", "archaic", "colloquial", "informal",
>   "figuratively", "figurative", "poetic", "uncommon", "neologism",
>   "obsolete form"]

Combine these together into a set of all labels we want to ignore.

> ignoredLabels :: HashSet Text
> ignoredLabels = syntacticLabels <> grammarLabels <> usageLabels

> ignoreLabel :: Text -> Bool
> ignoreLabel label = (elem label ignoredLabels) ||
>                     (any (\prefix -> isPrefixOf prefix label) mentionPrefixes)

> handleLabelTemplate :: Template -> AnnotatedText
> handleLabelTemplate template =
>   let entries     = map (\arg -> get arg template) ["2", "3", "4", "5"]
>       goodEntries = filter (not . ignoreLabel) entries
>       annotations = map labelToAnnotation goodEntries
>   in annotate annotations (asText "")
>
> labelToAnnotation :: Text -> Annotation
> labelToAnnotation label = mapFromList [("rel", "context"), ("language", "en"), ("page", label)]

Qualifiers are similar to labels, but take just a single argument.

> handleQualifierTemplate :: Template -> AnnotatedText
> handleQualifierTemplate template =
>   let label = get "1" template in
>     if (ignoreLabel label)
>       then mempty
>       else annotate [labelToAnnotation label] ""

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

Starting in this section, we get to make the most of our little
`annotationBuilder` language for expressing how to convert templates into
AnnotatedText.

This little language is defined in the documentation for
`Text.MediaWiki.Wiktionary.Base`, but here's a summary:

- `put` _key_ _value_: put the actual value _value_ in the annotation named
  _key_. This is particularly used for constants.

- `adapt` _key_ _keylist_ _template_: try looking up each key in _keylist_ in
  the _template_, in order. When we find such a key that exists, its value will
  become the value of the annotation named `key`.

- `visible` _keylist_ _template_: like `adapt`, but it sets the text of the
  AnnotatedText object to the value it finds. In other words, it says
  which of the template values becomes the text that would be visible to
  someone reading the article.

- `invisible`: states that this template has no visible text, so we should just
  produce the empty string plus annotations.

TODO: describe what these template parsers are for.

> handleLinkTemplate :: Template -> AnnotatedText
> handleLinkTemplate t = annotationBuilder $ do
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   adapt "gloss" ["4", "gloss"] t
>   adapt "pos" ["pos"] t
>   visible ["3", "2"] t
>
> handleDerivationTemplate :: Template -> AnnotatedText
> handleDerivationTemplate t = annotationBuilder $ do
>   put "rel" "*derived/etym"
>   adapt "language" arg2 t
>   adapt "page" arg3 t
>   visible arg3 t
>
> handleEtylTemplate :: Template -> AnnotatedText
> handleEtylTemplate t = annotationBuilder $ do
>   put "rel" "*derived/etym"
>   -- The page name "-" means "figure out what word we're referring to from
>   -- context", which is basically always the case for {{etyl}}.
>   put "page" "-"
>   adapt "language" arg1 t
>   invisible

The `{{compound}}` template can look like this:

    {{compound|en|place|holder}}

where argument 1 is the language code, and arguments 2 and 3 are the terms
being compounded. Or it can look like this:

    {{compound|lang=en|place|holder}}

Which looks like it's just being more specific, but now, argument "lang" is the
language code, and arguments 1 and 2 are the terms being compounded. Am I just
counting positional arguments wrong? Nope, the template can even appear like
this:

    {{compound|place|holder|lang=en}}

Which makes it entirely clear that "place" and "holder" are arguments 1 and 2.

I don't know if there is any unified logic to the implementation of this
template that makes sense in MediaWiki-land, so we introduce `fixLanguageArg`,
which ensures that the first two non-language arguments are labeled "2" and "3".

> fixLanguageArg :: Template -> Template
> fixLanguageArg t =
>   if (hasKey "lang" t)
>     then [("lang", get "lang" t), ("2", get "1" t), ("3", get "2" t)]
>     else [("lang", get "1" t), ("2", get "2" t), ("3", get "3" t)]
>
> handleCompoundTemplate :: Template -> AnnotatedText
> handleCompoundTemplate t =
>   let t' = fixLanguageArg t
>       language = get "lang" t'
>       term1 = get "2" t'
>       term2 = get "3" t'
>       text = term1 <> " + " <> term2
>   in annotate
>      [mapFromList [("rel", "*derived"), ("language", language), ("page", term1)],
>       mapFromList [("rel", "*derived"), ("language", language), ("page", term2)]]
>      text

`{{prefix}}`, `{{suffix}}`, and `{{cognate}}` have a language argument that can move
in similar ways.

> handleCognateTemplate :: Template -> AnnotatedText
> handleCognateTemplate t = annotationBuilder $ do
>   let t' = fixLanguageArg t
>   put "rel" "related/etym"
>   adapt "language" ["lang"] t'
>   adapt "page" arg2 t'
>   visible arg2 t'
>
> handlePrefixTemplate :: Template -> AnnotatedText
> handlePrefixTemplate t = annotationBuilder $ do
>   let t' = fixLanguageArg t
>   put "rel" "*derived"
>   adapt "language" ["lang"] t'
>   adapt "page" arg3 t'
>   visible arg3 t'
>
> handleSuffixTemplate :: Template -> AnnotatedText
> handleSuffixTemplate t = annotationBuilder $ do
>   let t' = fixLanguageArg t
>   put "rel" "*derived"
>   adapt "language" ["lang"] t'
>   adapt "page" arg2 t'
>   visible arg2 t'

The `{{ja-r}}` template is used to create Japanese text with "ruby text" or
"furigana" -- small hiragana characters above the text to indicate how the
kanji are pronounced. In some cases, the `%` character is used to specify how
the hiragana and kanji should be aligned.

Here, we want to keep just the text as written in kanji, and discard the
furigana.

> handleJapaneseRubyTemplate :: Template -> AnnotatedText
> handleJapaneseRubyTemplate t =
>   let text = getPrioritized ["linkto", "1"] t
>       term = replace "%" "" text
>   in annotate
>      [mapFromList [("language", "ja"), ("page", term)]]
>      term

Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedText
> handleTranslationTemplate t = annotationBuilder $ do
>   put "rel" "translation"
>   adapt "language" arg1 t
>   adapt "page" arg2 t
>   visible ["alt", "2"] t


Form-of templates
-----------------

> handleAbstractFormTemplate :: Template -> AnnotatedText
> handleAbstractFormTemplate t = annotationBuilder $ do
>   put "rel" ("form/" <> (get "1" t))
>   adapt "form" arg1 t
>   adapt "page" arg2 t
>   adapt "language" ["lang"] t
>   invisible
>
> handleFormTemplate :: Text -> Template -> AnnotatedText
> handleFormTemplate form t = annotationBuilder $ do
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
>   in annotationBuilder $ do
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

`handleChineseFormTemplate` handles the box that identifies the simplified
and traditional forms of a word, possibly including multiple forms of each,
appearing at the top of an etymology section.

The head-word is assumed to be a traditional form, so the "t" parameter is
rarely used.

> handleChineseFormTemplate :: Template -> AnnotatedText
> handleChineseFormTemplate t =
>   let simpTexts = getAll ["s", "s2", "s3"] t
>       tradTexts = getAll ["t", "t2", "t3", "alt"] t
>       simpLinks = [annotationFromList [("rel", "synonym"), ("language", "zh-Hans"), ("page", text)]
>                                       | text <- simpTexts]
>       tradLinks = [annotationFromList [("rel", "synonym"), ("language", "zh-Hant"), ("page", text)]
>                                       | text <- tradTexts]
>   in annotate (simpLinks ⊕ tradLinks) ""
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
>   in annotationBuilder $ do
>       put "rel" ("form/" <> formStr)
>       put "language" "es"
>       adapt "page" ["1", "inf", "verb", "infinitive"] t
>       invisible
>
> handleSpanishCompoundTemplate t =
>   let forms = getAll ["mood", "person"] t
>       formStr = intercalate "+" ("compound":forms)
>   in annotationBuilder $ do
>       put "rel" ("form/" <> formStr)
>       put "language" "es"
>       adapt "page" ["3", "1"] t
>       invisible
>
> handleSwedishFormTemplate t =
>   let (formStart, formEnd) = breakOnEnd "-form-" (get "0" t)
>       formStr = replace "-" "+" formEnd
>   in annotationBuilder $ do
>       put "rel" ("form/" <> formStr)
>       put "language" "sv"
>       adapt "page" ["1"] t
>       invisible

There are many templates named "xx-verb form of", and they tend to work in
similar ways: several arguments to the template are symbols for various kinds
of inflections, and we just want to join these symbols together. We just need
to know which language it is, and which template arguments to look up.

> handleLanguageFormTemplate :: Language -> [Text] -> Template -> AnnotatedText
> handleLanguageFormTemplate language formKeys t =
>   let forms = getAll formKeys t
>       formStr = intercalate "+" forms
>   in annotationBuilder $ do
>     put "rel" ("form/" <> formStr)
>     put "language" (fromLanguage language)
>     adapt "page" arg1 t
>     invisible

> handleLanguageFormTemplate2 :: Language -> [Text] -> Template -> AnnotatedText
> handleLanguageFormTemplate2 language formKeys t =
>   let forms = getAll formKeys t
>       formStr = intercalate "+" forms
>   in annotationBuilder $ do
>     put "rel" ("form/" <> formStr)
>     put "language" (fromLanguage language)
>     adapt "page" arg2 t
>     invisible

Possible templates still to handle:

- [[Category:Form-of_templates]]
- feminine plural past participle of
- feminine singular past participle of
- masculine plural past participle of
- masculine singular past participle of
- neuter singular past participle of
- verbal noun of


The big template dispatcher
---------------------------

> enTemplates :: TemplateProc
> enTemplates "l"         = handleLinkTemplate
> enTemplates "link"      = handleLinkTemplate
> enTemplates "m"         = handleLinkTemplate
> enTemplates "mention"   = handleLinkTemplate
> enTemplates "inherited" = handleDerivationTemplate
> enTemplates "inh"       = handleDerivationTemplate
> enTemplates "derived"   = handleDerivationTemplate
> enTemplates "der"       = handleDerivationTemplate
> enTemplates "borrowed"  = handleDerivationTemplate
> enTemplates "borrowing" = handleDerivationTemplate
> enTemplates "bor"       = handleDerivationTemplate
> enTemplates "cognate"   = handleCognateTemplate
> enTemplates "cog"       = handleCognateTemplate
> enTemplates "ja-r"      = handleJapaneseRubyTemplate
> enTemplates "prefix"    = handlePrefixTemplate
> enTemplates "suffix"    = handleSuffixTemplate
> enTemplates "compound"  = handleCompoundTemplate
> enTemplates "blend"     = handleCompoundTemplate
> enTemplates "etyl"      = handleEtylTemplate
> enTemplates "label"     = handleLabelTemplate
> enTemplates "lbl"       = handleLabelTemplate
> enTemplates "lb"        = handleLabelTemplate
> enTemplates "qualifier" = handleQualifierTemplate
> enTemplates "sense"     = handleSenseTemplate
> enTemplates "senseid"   = handleSenseIDTemplate
> enTemplates "t"         = handleTranslationTemplate
> enTemplates "t+"        = handleTranslationTemplate
> enTemplates "t-"        = handleTranslationTemplate
> enTemplates "tø"        = handleTranslationTemplate
> -- ignore the more uncertain translation templates, t-check and t+check
>
> enTemplates "form of"             = handleAbstractFormTemplate
> enTemplates "alternative form of" = handleFormTemplate "alternate"
> enTemplates "alternate form of"   = handleFormTemplate "alternate"
> enTemplates "alternative spelling of" = handleFormTemplate "alternate"
> enTemplates "misspelling of"      = handleFormTemplate "alternate"
> enTemplates "alt form of"         = handleFormTemplate "alternate"
> enTemplates "alt form"            = handleFormTemplate "alternate"
> enTemplates "altform"             = handleFormTemplate "alternate"
> enTemplates "inflection of"       = handleInflectionTemplate
> enTemplates "conjugation of"      = handleInflectionTemplate
>
> -- templates that take unbounded numbers of arguments
> enTemplates "rel2" = handleUnboundedTemplate "related" "en"
> enTemplates "rel3" = handleUnboundedTemplate "related" "en"
> enTemplates "rel4" = handleUnboundedTemplate "related" "en"
> enTemplates "der2" = handleUnboundedTemplate "derived" "en"
> enTemplates "der3" = handleUnboundedTemplate "derived" "en"
> enTemplates "der4" = handleUnboundedTemplate "derived" "en"
> enTemplates "der5" = handleUnboundedTemplate "derived" "en"
> enTemplates "zh-der" = handleUnboundedTemplate "derived" "zh"
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
> enTemplates "second-person singular past of"       = handleFormTemplate "archaic+2+s+past"
> enTemplates "plural of"                            = handleFormTemplate "p"
> enTemplates "gerund of"                            = handleFormTemplate "ger"
> enTemplates "imperative of"                        = handleFormTemplate "imp"
> enTemplates "reflexive of"                         = handleFormTemplate "ref"
> enTemplates "singular definite of"                 = handleFormTemplate "s+def"
> enTemplates "plural definite of"                   = handleFormTemplate "p+def"
> enTemplates "singular indefinite of"               = handleFormTemplate "s+indef"
> enTemplates "plural indefinite of"                 = handleFormTemplate "p+indef"
> enTemplates "en-third-person singular of"          = handleSpecificFormsTemplate "en" ["3+s+pres"]
> enTemplates "en-third person singular of"          = handleSpecificFormsTemplate "en" ["3+s+pres"]
> enTemplates "en-archaic second-person singular of" = handleSpecificFormsTemplate "en" ["archaic+2+s+pres"]
> enTemplates "en-archaic third-person singular of"  = handleSpecificFormsTemplate "en" ["archaic+3+s+pres"]
>
> enTemplates "en-irregular plural of"      = handleSpecificFormsTemplate "en" ["p"]
> enTemplates "en-comparative of"           = handleSpecificFormsTemplate "en" ["comp"]
> enTemplates "en-superlative of"           = handleSpecificFormsTemplate "en" ["sup"]
>
> enTemplates "de-inflected form of"        = handleSpecificFormsTemplate "de" ["?"]
> enTemplates "de-zu-infinitive of"         = handleSpecificFormsTemplate "de" ["zu"]
> enTemplates "de-verb form of"             = handleLanguageFormTemplate "de" ["2","3","4","5"]
> enTemplates "nl-noun form of"             = handleLanguageFormTemplate2 "nl" ["1"]
> enTemplates "nl-adj form of"              = handleLanguageFormTemplate2 "nl" ["1"]
> enTemplates "nl-verb form of"             = handleLanguageFormTemplate "nl" ["p","n","t","m"]
>
> enTemplates "es-verb form of"      = handleSpanishVerbTemplate
> enTemplates "es-compound of"       = handleSpanishCompoundTemplate
>
> enTemplates "pt-noun form of" = handleLanguageFormTemplate "pt" ["2","3","4"]
> enTemplates "pt-adj form of"  = handleLanguageFormTemplate "pt" ["2","3","4"]
> enTemplates "pt-verb form of" = handleLanguageFormTemplate "pt" ["3","4","5","6"]
>
> enTemplates "fi-form of"       = handleLanguageFormTemplate "fi" ["case","pr","pl","mood","tense","suffix"]
> enTemplates "ru-participle of" = handleLanguageFormTemplate "ru" ["2","3","4","5"]
>
> enTemplates x
>   | isPrefixOf "sv-noun-form" x = handleSwedishFormTemplate
>   | isPrefixOf "sv-adj-form"  x = handleSwedishFormTemplate
>   | isPrefixOf "sv-verb-form" x = handleSwedishFormTemplate
>   | otherwise                   = skipTemplate
