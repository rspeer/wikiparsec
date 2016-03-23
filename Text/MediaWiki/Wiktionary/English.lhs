> {-# LANGUAGE OverloadedStrings #-}
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


Parsing sections
================

> enHandlePage :: ByteString -> ByteString -> [WiktionaryRel]
> enHandlePage title text =
>   let sections = parsePageIntoSections text in
>     concat (map (enDispatchSection title) sections)
>
> enHandleFile :: ByteString -> String -> IO ()
> enHandleFile title filename = do
>   contents <- Char8.readFile filename
>   mapM_ print (enHandlePage title contents)

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
>       language = headings !! 1;
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
> enParseSectionContent x = const (const [])


The part-of-speech/definition section
-------------------------------------

First, make a specific version of the function that extracts relationships
from the text of a definition:

> enDefinitionToRels = definitionToRels "en"

Parsing the definition section. (TODO: adjust the term with a sense number?)

> enParseDefinition :: WiktionaryTerm -> ByteString -> [WiktionaryRel]
> enParseDefinition thisTerm text =
>   let defs = parseOrDefault [] pDefinition text in
>     concat (map (definitionToRels "en" thisTerm) defs)
>
> pDefinition :: Parser [(ByteString, AnnotatedString)]
> pDefinition = do
>   -- Skip miscellaneous lines at the start of the section, including
>   -- the template that looks like {{en-noun}} or whatever
>   textChoices [templateText enTemplates, newLine]
>   defList <- orderedList enTemplates "#"
>   return (extractNumberedDefs defList)


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

These labels provide grammatical (not semantic) information:

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

> enLabelAnnotation :: ByteString -> Annotation
> enLabelAnnotation label = [("rel", labelType label), ("language", "en"), ("page", label)]
>
> labelType :: ByteString -> ByteString
> labelType label = if elem label grammarLabels then "grammar-label" else "context-label"
>
> handleLabelTemplate :: Template -> AnnotatedString
> handleLabelTemplate template =
>   let {
>     entries     = map (\arg -> get arg template) ["2", "3", "4", "5"];
>     goodEntries = filter (\val -> not (elem val ignoredLabels)) entries;
>     annotations = map enLabelAnnotation goodEntries
>   } in A.annotate annotations ""

Links
-----

> handleLinkTemplate :: Template -> AnnotatedString
> handleLinkTemplate template =
>   let { linkText = (getOne ["3", "2"] template);
>         annot = filterEmpty $
>           [("language", (get "1" template)),
>            ("page", (get "2" template)),
>            ("gloss", (getOne ["4", "gloss"] template)),
>            ("pos", (get "pos" template))] }
>   in (A.annotate [annot] linkText)

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
> enTemplates _         = skipTemplate
