> {-# LANGUAGE OverloadedStrings #-}
> module Text.MediaWiki.Wiktionary.English where
> import Text.MediaWiki.Templates
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AList (get, filterEmpty, lookupOne, getOne, ByteAssoc)
> import Text.MediaWiki.AnnotatedString (AnnotatedString, Annotation)
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Sections
> import Text.MediaWiki.Wiktionary.Base
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as Char8
> import Data.Attoparsec.ByteString.Char8
> import Data.LanguageNames
> import Control.Applicative ((<|>), (<$>), (*>), (<*))
> import Control.Monad


Parsing sections
================

> enHandlePage :: ByteString -> ByteString -> [WiktionaryRel]
> enHandlePage title text =
>   let sections = parsePageIntoSections text in
>     concat (map (enDispatchSection title) sections)
>
> enHandleFile :: String -> String -> IO ()
> enHandleFile title filename = do
>   contents <- Char8.readFile filename
>   mapM_ print (enHandlePage (Char8.pack title) contents)

Choosing an appropriate section parser
--------------------------------------

> fakeRel :: ByteString -> ByteString -> WiktionaryRel
> fakeRel s1 s2 = makeRel "debug" (simpleTerm "en" s1) (simpleTerm "en" s2)
>
> enDispatchSection :: ByteString -> WikiSection -> [WiktionaryRel]
> enDispatchSection title (WikiSection {headings=headings, content=content}) =
>   if (length headings) < 3
>     then []
>     else let {
>       language = lookupLanguage "en" (headings !! 1);
>       subheads = drop 2 headings;
>       maybePos = enFindPartOfSpeech subheads;
>       etymNumber = enFindEtymologyNumber subheads;
>       sectionType = enGetSectionType (last subheads)
>     } in case maybePos of
>       Just pos -> enParseSection language title pos etymNumber sectionType content
>       Nothing  -> []
>
> enParseSection :: Language -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> [WiktionaryRel]
> enParseSection language title pos etymNumber sectionType content =
>   let {
>     thisTerm = WiktionaryTerm { text=title, language=Just language, etym=Just etymNumber, pos=Just pos, sense=Nothing }
>   } in enParseSectionContent sectionType thisTerm content
>
> enParseSectionContent :: ByteString -> WiktionaryTerm -> ByteString -> [WiktionaryRel]
> enParseSectionContent "POS" = enParseDefinition
> enParseSectionContent "Translations" = enParseTranslations
> enParseSectionContent x = const (const [])


The part-of-speech/definition section
-------------------------------------

First, make a specific version of the function that extracts relationships
from the text of a definition:

> enDefinitionToRels = definitionToRels "en"

Parsing the definition section:

> enParseDefinition :: WiktionaryTerm -> ByteString -> [WiktionaryRel]
> enParseDefinition thisTerm text =
>   let defs = parseOrDefault [] pDefinitionSection text in
>     concat (map (definitionToRels "en" thisTerm) defs)
>
> pDefinitionSection :: Parser [(ByteString, AnnotatedString)]
> pDefinitionSection = do
>   -- Skip miscellaneous lines at the start of the section, including
>   -- the template that looks like {{en-noun}} or whatever
>   optionalTextChoices [templateText enTemplates, newLine]
>   defList <- orderedList enTemplates "#"
>   return (extractNumberedDefs defList)


The translation section
-----------------------

> enParseTranslations :: WiktionaryTerm -> ByteString -> [WiktionaryRel]
> enParseTranslations thisTerm text =
>   let translationMaybe = parseOnly (pTranslationSection thisTerm) text in
>     case translationMaybe of
>       Right translations -> translations
>       Left err -> error err
>
> pTranslationSection :: WiktionaryTerm -> Parser [WiktionaryRel]
> pTranslationSection thisTerm = concat <$> many1 (pTranslationGroup thisTerm)
> 
> pTranslationGroup :: WiktionaryTerm -> Parser [WiktionaryRel]
> pTranslationGroup thisTerm = do
>   optionalTextChoices [newLine]
>   maybeSense <- pTranslationTopTemplate
>   let senseTerm = thisTerm {sense=maybeSense}
>   items <- concat <$> many1 pTranslationColumn
>   optionalTextChoices [newLine]
>   return (map (annotationToRel senseTerm) (filter translationsOnly items))
>
> translationsOnly :: Annotation -> Bool
> translationsOnly annot = (get "rel" annot) == "translation"

The `pTranslationTopTemplate` rule parses the template that starts a
translation section, which may or may not be labeled with a word sense. It
returns a Maybe ByteString that contains the word sense if present.

> pTranslationTopTemplate :: Parser (Maybe ByteString)
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

  - `extractTopLevel` gave us a list of AnnotatedStrings. We want just their
    annotations, representing everything we want to know about the
    translations, in one big list. So we `A.concat` all the AnnotatedStrings
    together, and then take the combined list of annotations from that.

> pTranslationItem :: Parser [Annotation]
> pTranslationItem = A.annotations <$> A.concat <$> extractTopLevel <$> listItem enTemplates "*"
>
> pTranslationBlankLine :: Parser [Annotation]
> pTranslationBlankLine = newLine >> return []

Finding headings
================

The following list of parts of speech comes from:
https://en.wiktionary.org/wiki/Wiktionary:Entry_layout#Part_of_speech

We skip entries in the sub-lists called 'Symbols and characters', 'Han
characters and language-specific varieties', and 'Lojban-specific parts of
speech'.

> partsOfSpeech :: [ByteString]
> partsOfSpeech = [
>   "Adjective", "Adverb", "Ambiposition", "Article", "Circumposition",
>   "Classifier", "Conjunction", "Contraction", "Counter", "Determiner",
>   "Interjection", "Noun", "Numeral", "Participle", "Particle",
>   "Postposition", "Preposition", "Pronoun", "Proper noun", "Verb",
>   "Circumfix", "Combining form", "Infix", "Interfix", "Prefix", "Root",
>   "Suffix", "Phrase", "Proverb", "Prepositional phrase"
>   ]
>
> enFindPartOfSpeech :: [ByteString] -> Maybe ByteString
> enFindPartOfSpeech = findHeading partsOfSpeech
>
> enFindEtymologyNumber :: [ByteString] -> ByteString
> enFindEtymologyNumber headings =
>   case findPrefixedHeading "Etymology " headings of
>     Just x -> x
>     Nothing -> "1"

Generalizing the type of a heading:

> enGetSectionType :: ByteString -> ByteString
> enGetSectionType heading =
>   if elem heading partsOfSpeech
>     then "POS"
>     else if Char8.isPrefixOf "Etymology " heading
>       then "Etymology"
>       else heading


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

> ignoredLabels :: [ByteString]
> ignoredLabels = [
>   "", "and", "&", "or", "_", "now", "except", "except in", "outside",
>   "especially", "chiefly", "mainly", "mostly", "particularly", "primarily",
>   "excluding", "extremely", "frequently", "including", "literally",
>   "literal", "many", "markedly", "mildly", "now", "nowadays", "of", "of a",
>   "of an", "often", "originally", "possibly", "rarely", "slightly",
>   "sometimes", "somewhat", "strongly", "typically", "usually", "very"]

These labels provide grammatical (not semantic) information. We'll keep them
separate in case we ever want to output them:

> grammarLabels :: [ByteString]
> grammarLabels = [ "abbreviation", "acronym", "active", "active voice",
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

> handleLabelTemplate :: Template -> AnnotatedString
> handleLabelTemplate template =
>   let {
>     entries     = map (\arg -> get arg template) ["2", "3", "4", "5"];
>     goodEntries = filter (\val -> not (elem val ignoredLabels || elem val grammarLabels)) entries;
>     annotations = map enLabelAnnotation goodEntries
>   } in A.annotate annotations ""
>
> enLabelAnnotation :: ByteString -> Annotation
> enLabelAnnotation label = [("rel", "context"), ("language", "en"), ("page", label)]


Sense IDs
---------
Hooray! Wiktionary is finally starting to mark word senses with stable IDs
instead of just numbered lists. But we need to be able to extract that
information, mapping the sense number to the sense.

We'll make a "senseID" annotation that we pass on to the definition parser in
Wiktionary.Base.

> handleSenseIDTemplate :: Template -> AnnotatedString
> handleSenseIDTemplate template = A.annotate [[("senseID", get "2" template)]] ""


Links
-----

> handleLinkTemplate :: Template -> AnnotatedString
> handleLinkTemplate template =
>   let text  = (getOne ["3", "2"] template)
>       annot = filterEmpty $
>         [("language", (get "1" template)),
>          ("page", (get "2" template)),
>          ("gloss", (getOne ["4", "gloss"] template)),
>          ("pos", (get "pos" template))]
>   in (A.annotate [annot] text)


Translations
------------

> handleTranslationTemplate :: Template -> AnnotatedString
> handleTranslationTemplate template =
>   let annot = [("rel", "translation"),
>                ("language", (get "1" template)),
>                ("page", (get "2" template))]
>       text  = getOne ["alt", "2"] template
>   in A.annotate [annot] text


Form-of templates
-----------------

> handleAbstractFormTemplate :: Template -> AnnotatedString
> handleAbstractFormTemplate template =
>   let text  = (getOne ["3", "2"] template)
>       annot = filterEmpty $
>         [("rel", "form"),
>          ("language", (get "lang" template)),
>          ("page", (get "2" template)),
>          ("form", (get "1" template))]
>   in A.annotate [annot] text
>
> handleFormTemplate :: ByteString -> Template -> AnnotatedString
> handleFormTemplate form template =
>   let text  = (getOne ["2", "1"] template)
>       annot = filterEmpty $
>         [("rel", "form"),
>          ("language", (get "lang" template)),
>          ("page", (get "1" template)),
>          ("form", form)]
>   in A.annotate [annot] text
>
> handleInflectionTemplate :: Template -> AnnotatedString
> handleInflectionTemplate template =
>   let text  = (getOne ["2", "1"] template)
>       forms = filter (/= "") [get "3" template, get "4" template, get "5" template, get "6" template]
>       annot = filterEmpty $
>         [("rel", "form"),
>          ("language", (get "lang" template)),
>          ("page", (get "1" template)),
>          ("form", Char8.intercalate "|" forms)]
>   in A.annotate [annot] text

We need to handle:

- en-comparative of
- en-simple past of
- en-irregular plural of
- en-past of
- en-superlative of
- en-third-person singular of

- active participle of
- feminine plural past participle of
- feminine singular past participle of
- future participle of
- gerund of
- imperative of
- masculine plural past participle of
- neuter singular past participle of
- participle of
- passive of
- passive participle of
- past active participle of
- past participle of
- past passive participle of
- past tense of
- present active participle of
- present participle of
- present passive participle of
- present tense of
- reflexive of
- second-person singular past of
- (x) supine of
- verbal noun of

- plural of
- past tense of
- past participle of


Putting it all together
-----------------------

> enTemplates :: TemplateProc
> enTemplates "l"       = handleLinkTemplate
> enTemplates "link"    = handleLinkTemplate
> enTemplates "m"       = handleLinkTemplate
> enTemplates "mention" = handleLinkTemplate
> enTemplates "label"   = handleLabelTemplate
> enTemplates "lbl"     = handleLabelTemplate
> enTemplates "lb"      = handleLabelTemplate
> enTemplates "senseid" = handleSenseIDTemplate
> enTemplates "t"       = handleTranslationTemplate
> enTemplates "t+"      = handleTranslationTemplate
> enTemplates "t-"      = handleTranslationTemplate
> enTemplates "tø"      = handleTranslationTemplate
> enTemplates "form of" = handleAbstractFormTemplate
> enTemplates "alternative form of" = handleFormTemplate "alternate"
> enTemplates "alternate form of"   = handleFormTemplate "alternate"
> enTemplates "alt form of"         = handleFormTemplate "alternate"
> enTemplates "alt form"            = handleFormTemplate "alternate"
> enTemplates "altform"             = handleFormTemplate "alternate"
> enTemplates "inflection of"       = handleInflectionTemplate
> enTemplates "conjugation of"      = handleInflectionTemplate
> -- ignore the more uncertain translation templates, t-check and t+check
> enTemplates _         = skipTemplate

