This file defines how to parse Wiktionary entries, as a layer above the basic
handling of wiki syntax in `Wikitext.lhs`.

> module Text.MediaWiki.Wiktionary where
> import Text.MediaWiki.Wikitext
> import Text.Parsec.Char



A WiktionaryTerm is a piece of text that can be defined on Wiktionary. It is
defined by its term text, the language it's in (which may be unknown), and
a string that identifies its word sense (which may be unknown or missing).

Languages are provided as rather un-standardized strings. It's outside the
scope of this Haskell code to recognize BCP 47 language codes and their
corresponding names (we'll be using the `langcodes` module in Python for that
later). So English could appear as `en`, `eng`, `en-US`, or `English`
interchangeably.

> data WiktionaryTerm = WiktionaryTerm {
>   text :: String,
>   language :: Maybe String,
>   sense :: Maybe String
> } deriving (Show, Eq)

A WiktionaryRel expresses a relationship between terms that we can extract
from a page.

> data WiktionaryRel = WiktionaryRel {
>   relation :: String,
>   fromTerm :: WiktionaryTerm,
>   toTerm :: WiktionaryTerm
> } deriving (Show, Eq)

A simple way to convert a WikiLink to a WiktionaryTerm:

> linkToTerm :: WikiLink -> WiktionaryTerm
> linkToTerm link = WiktionaryTerm { text=(page link), language=(section link), sense=Nothing }

Parse a section by concatenating the information from several subsections:

> combineSections :: [Parser [WiktionaryRel]] -> Parser [WiktionaryRel]
> combineSections options = do
>   sections <- many (choice (map try options))
>   return (concat sections)


Language sections
-----------------
yeah this definitely doesn't work anymore

> languageSection :: String -> Parser [WiktionaryRel]
> languageSection pageName = do
>   languageName <- heading 2
>   let term = WiktionaryTerm { text=pageName, language=languageName } in
>     combineSections [
>       etymologyGroup term 3, etymologySection term 3,
>       partOfSpeechSection term 3, miscellaneousSection term 3
>     ]


Etymology sections
------------------

> etymologyGroup :: WiktionaryTerm -> Int -> Parser [WiktionaryRel]
> etymologyGroup term level = do
>   etymHeading <- specificHeading level etymologyLabel
>   etymRels <- etymologyContent
>   moreRels <- combineSections [partOfSpeechSection term 4, miscellaneousSection term 4]
>   return (etymRels ++ moreRels)

> numericLabel :: Parser String
> numericLabel = many1 digit

> etymologyLabel :: Parser String
> etymologyLabel = do
>   symbol "Etymology"
>   sameLineSpaces
>   numericLabel

> etymologySection :: WiktionaryTerm -> Int -> Parser [WiktionaryRel]
> etymologySection term level = do
>   specificHeading level (symbol "Etymology")
>   etymologyContent term

TODO: etymologyContent

Part of speech sections
-----------------------

This list comes from: https://en.wiktionary.org/wiki/Wiktionary:Entry_layout#Part_of_speech

We skip entries in the sub-lists called 'Symbols and characters', 'Han
characters and language-specific varieties', and 'Lojban-specific parts of
speech', as we don't intend to extract information from sections of those
types.

> PARTS_OF_SPEECH = [
>   "Adjective", "Adverb", "Ambiposition", "Article", "Circumposition",
>   "Classifier", "Conjunction", "Contraction", "Counter", "Determiner",
>   "Interjection", "Noun", "Numeral", "Participle", "Particle",
>   "Postposition", "Preposition", "Pronoun", "Proper noun", "Verb",
>   "Circumfix", "Combining form", "Infix", "Interfix", "Prefix", "Root",
>   "Suffix", "Phrase", "Proverb", "Prepositional phrase"
> ]
> partOfSpeechLabel :: Parser String
> partOfSpeechLabel = foldl <|> (map symbol PARTS_OF_SPEECH)
>
> definitionList = do
>   listItems <- (orderedList "#")
>   return (map linkToTerm (filter isPlainItem listItems))
>
> makeRel :: String -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryRel
> makeRel rel from to = WiktionaryRel { relation=rel, fromTerm=from, toTerm=to }
> makeGenericRel = makeRel "RelatedTo"
>
> partOfSpeechSection :: WiktionaryTerm -> Int -> Parser [WiktionaryTerm]
> partOfSpeechSection term level = do
>   partOfSpeech <- specificHeading level partOfSpeechLabel
>   many basicLine
>   map (makeGenericRel term) (returnStateFrom definitionList)

