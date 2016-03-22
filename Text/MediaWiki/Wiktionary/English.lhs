> {-# LANGUAGE OverloadedStrings #-}
> import Text.MediaWiki.Templates
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AnnotatedString (AnnotatedString, Annotation, get, filterEmpty, lookupOne)
> import Text.MediaWiki.ParseTools
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Sections
> import Text.MediaWiki.Wiktionary.Base


Parsing sections
================

Choosing an appropriate section parser
--------------------------------------

> enDispatchSection :: ByteString -> WikiSection -> [WiktionaryRel]
> enDispatchSection term (WikiSection {headings=headings, content=content}) =
>   if (length headings) < 3
>     then []
>     else let {
>       language = headings !! 1;
>       subheads = drop 2 headings;
>       maybePos = enFindPartOfSpeech subheads;
>       etymNumber = enFindEtymologyNumber subheads;
>       sectionType = enGetSectionType (last subheads)
>     } in case maybePos of
>       Just pos -> enParseSection language term pos etymNumber sectionType content
>       Nothing  -> []
>
> enParseSection :: Language -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> [WiktionaryRel]
> enParseSection language term pos etymNumber sectionType content =
>   let {
>     partialSense = pos ++ "/" ++ etymNumber;
>     thisTerm = WiktionaryTerm { text=term, language=Just language, sense=partialSense }
>   } in enParseSectionContent sectionType thisTerm content
>
> enParseSectionContent :: ByteString -> WiktionaryTerm -> ByteString -> [WiktionaryRel]
> enParseSectionContent "POS" = enParseDefinition
> enParseSectionContent _ = const (const [])


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
> pDefinition :: Parser [AnnotatedString]
> pDefinition = do
>   -- Skip miscellaneous lines at the start of the section, including
>   -- the template that looks like {{en-noun}} or whatever
>   concatMany [wikiTextLine enTemplates, newLine]
>   defList <- orderedList enTemplates "#"
>   return (extractTopLevel defList)


Finding headings
================

The following list of parts of speech comes from:
https://en.wiktionary.org/wiki/Wiktionary:Entry_layout#Part_of_speech

We skip entries in the sub-lists called 'Symbols and characters', 'Han
characters and language-specific varieties', and 'Lojban-specific parts of
speech'.

> PARTS_OF_SPEECH :: [ByteString]
> PARTS_OF_SPEECH = [
>   "Adjective", "Adverb", "Ambiposition", "Article", "Circumposition",
>   "Classifier", "Conjunction", "Contraction", "Counter", "Determiner",
>   "Interjection", "Noun", "Numeral", "Participle", "Particle",
>   "Postposition", "Preposition", "Pronoun", "Proper noun", "Verb",
>   "Circumfix", "Combining form", "Infix", "Interfix", "Prefix", "Root",
>   "Suffix", "Phrase", "Proverb", "Prepositional phrase"
> ]
>
> enFindPartOfSpeech :: [ByteString] -> Maybe ByteString
> enFindPartOfSpeech = findHeading PARTS_OF_SPEECH
>
> enFindEtymologyNumber :: [ByteString] -> ByteString
> enFindEtymologyNumber headings =
>   case findPrefixedHeading "Etymology " headings of
>     Just x -> x
>     Nothing -> "1"

Generalizing the type of a heading:

> enGetSectionType :: ByteString -> ByteString
> enGetSectionType heading =
>   if elem heading PARTS_OF_SPEECH
>     then "POS"
>     else if isPrefixOf "Etymology " heading
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

> IGNORED_LABELS = [
>   "", "and", "&", "or", "_", "now", "except", "except in", "outside",
>   "especially", "chiefly", "mainly", "mostly", "particularly", "primarily",
>   "excluding", "extremely", "frequently", "including", "literally",
>   "literal", "many", "markedly", "mildly", "now", "nowadays", "of", "of a",
>   "of an", "often", "originally", "possibly", "rarely", "slightly",
>   "sometimes", "somewhat", "strongly", "typically", "usually", "very"]

These labels provide grammatical (not semantic) information:

> GRAMMAR_LABELS = [ "abbreviation", "acronym", "active", "active voice",
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
>   "usually in the plural", "usually in plural"]

> enLabelAnnotation :: ByteString -> Annotation
> enLabelAnnotation label = [("rel", labelType label), ("language", "en"), ("page", label)]
>
> labelType label :: ByteString -> ByteString
> labelType label = if elem label GRAMMAR_LABELS then "grammar-label" else "context-label"
>
> handleLabelTemplate template =
>   let {
>     entries     = map (\arg -> get arg template) ["1", "2", "3", "4"];
>     goodEntries = filter (\val -> not (elem val IGNORED_LABELS)) entries;
>     annotations = map enLabelAnnotation goodEntries
>   } in annotate annotations ""

Links
-----

> handleLinkTemplate :: AList -> AnnotatedString
> handleLinkTemplate template =
>   let { linkText = (getOne ["3", "2"] template),
>         annot = filterEmpty $
>           [("language", (get "1" template)),
>            ("page", (get "2" template)),
>            ("gloss", (getOne ["4", "gloss"] template)),
>            ("pos", (get "pos" template)] }
>   in (annotate [annot] linkText)

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
