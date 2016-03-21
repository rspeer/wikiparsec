> {-# LANGUAGE OverloadedStrings #-}

This file defines how to parse Wiktionary entries, as a layer above the basic
handling of wiki syntax in `Wikitext.lhs`.

> module Text.MediaWiki.Wiktionary where
> import Text.MediaWiki.WikiText
> import qualified Text.MediaWiki.AnnotatedString as A
> import qualified Text.MediaWiki.AnnotatedString (Annotation, AnnotatedString)
> import Text.MediaWiki.AList
> import Data.Attoparsec.ByteString.Char8
> import Data.Attoparsec.Combinator
> import qualified Data.ByteString (ByteString)
> import Control.Applicative ((<|>), (<$>), (*>), (<*))


A WiktionaryTerm is a piece of text that can be defined on Wiktionary. It is
defined by its term text, the language it's in (which may be unknown), and
a string that identifies its word sense (which may be unknown or missing).

Languages are provided as rather un-standardized strings. It's outside the
scope of this Haskell code to recognize BCP 47 language codes and their
corresponding names (we'll be using the `langcodes` module in Python for that
later). So English could appear as `en`, `eng`, `en-US`, or `English`
interchangeably.

> data WiktionaryTerm = WiktionaryTerm {
>   text :: ByteString,
>   language :: Maybe ByteString,
>   sense :: Maybe ByteString
> } deriving (Show, Eq)

A WiktionaryRel expresses a relationship between terms that we can extract
from a page.

> data WiktionaryRel = WiktionaryRel {
>   relation :: ByteString,
>   fromTerm :: WiktionaryTerm,
>   toTerm :: WiktionaryTerm
> }
>
> makeRel :: String -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryRel
> makeRel rel from to = WiktionaryRel { relation=rel, fromTerm=from, toTerm=to }
> makeGenericRel = makeRel "RelatedTo"


Converting an Annotation representing a term to a WiktionaryTerm:

> linkToTerm :: Annotation -> WiktionaryTerm
> linkToTerm link = WiktionaryTerm {
>   text=(get "page" link),
>   language=(lookupMaybe ["language", "section"] link),
>   sense=(lookup "sense" link)
> }
>
> nonempty :: ByteString -> Maybe ByteString
> nonempty "" = Nothing
> nonempty x  = Just x


TODO: possibly we should look up the language code at this point.




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

leftover code:

definitionList = do
  listItems <- (orderedList "#")
  ...

