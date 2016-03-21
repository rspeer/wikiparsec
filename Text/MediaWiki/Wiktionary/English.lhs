> {-# LANGUAGE OverloadedStrings #-}
> import Text.MediaWiki.Templates
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AnnotatedString (AnnotatedString, Annotation, get, filterEmpty, lookupOne)
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.Wiktionary.Base

Part of speech sections
-----------------------

This list comes from: https://en.wiktionary.org/wiki/Wiktionary:Entry_layout#Part_of_speech

We skip entries in the sub-lists called 'Symbols and characters', 'Han
characters and language-specific varieties', and 'Lojban-specific parts of
speech', as we don't intend to extract information from sections of those
types.

> PARTS_OF_SPEECH :: [ByteString]
> PARTS_OF_SPEECH = [
>   "Adjective", "Adverb", "Ambiposition", "Article", "Circumposition",
>   "Classifier", "Conjunction", "Contraction", "Counter", "Determiner",
>   "Interjection", "Noun", "Numeral", "Participle", "Particle",
>   "Postposition", "Preposition", "Pronoun", "Proper noun", "Verb",
>   "Circumfix", "Combining form", "Infix", "Interfix", "Prefix", "Root",
>   "Suffix", "Phrase", "Proverb", "Prepositional phrase"
> ]


Template handling
-----------------

> handleLinkTemplate template =
>   let { linkText = (getOne ["3", "2"] template),
>         annot = filterEmpty $
>           [("language", (get "1" template)),
>            ("page", (get "2" template)),
>            ("gloss", (getOne ["4", "gloss"] template)),
>            ("pos", (get "pos" template)] }
>   in (annotate annot linkText)
>
> enTemplates :: TemplateProc
> enTemplates "l"       = handleLinkTemplate
> enTemplates "link"    = handleLinkTemplate
> enTemplates "m"       = handleLinkTemplate
> enTemplates "mention" = handleLinkTemplate
> enTemplates _         = skipTemplate
