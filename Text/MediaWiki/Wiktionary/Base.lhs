> {-# LANGUAGE OverloadedStrings #-}

This file defines how to parse Wiktionary entries, as a layer above the basic
handling of wiki syntax in `Wikitext.lhs`.

> module Text.MediaWiki.Wiktionary.Base where
> import Text.MediaWiki.WikiText
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AnnotatedString (Annotation, AnnotatedString)
> import Text.MediaWiki.AList
> import Data.Attoparsec.ByteString.Char8
> import Data.Attoparsec.Combinator
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.UTF8 as UTF8
> import qualified Data.ByteString.Char8 as Char8
> import Control.Applicative ((<|>), (<$>), (*>), (<*))
> import Data.List (intersect)
> import Data.Maybe

Data types
----------
Here's a simple one: a Language is a ByteString, abstracted so we can possibly
change it later.

> type Language = ByteString

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
>   language :: Maybe Language,
>   sense :: Maybe ByteString,
>   pos :: Maybe ByteString,
>   etym :: Maybe ByteString
> } deriving (Eq)
>
> instance Show WiktionaryTerm where
>   show term =
>     let {
>       pieces = map (fromMaybe (UTF8.fromString "?"))
>                    [language term, Just (text term), etym term, pos term, sense term]
>     } in UTF8.toString (Char8.intercalate "/" pieces)
>
> simpleTerm language text = WiktionaryTerm {
>   text=text, language=Just language, sense=Nothing, pos=Nothing, etym=Nothing
>   }

A WiktionaryRel expresses a relationship between terms that we can extract
from a page.

> data WiktionaryRel = WiktionaryRel {
>   relation :: ByteString,
>   fromTerm :: WiktionaryTerm,
>   toTerm :: WiktionaryTerm
> } deriving (Show, Eq)
>
> makeRel :: ByteString -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryRel
> makeRel rel from to = WiktionaryRel { relation=rel, fromTerm=from, toTerm=to }
> makeGenericRel = makeRel "RelatedTo"


Converting an Annotation representing a term to a WiktionaryTerm:

> annotationToTerm :: Annotation -> WiktionaryTerm
> annotationToTerm annot = WiktionaryTerm {
>   text=(get "page" annot),
>   language=(lookupOne ["language", "section"] annot),
>   pos=(lookup "pos" annot),
>   sense=(lookup "sense" annot),
>   etym=(lookup "etym" annot)
>   }
>
> annotationToRel :: WiktionaryTerm -> Annotation -> WiktionaryRel
> annotationToRel thisTerm annot =
>   makeRel (getDefault "link" "rel" annot) thisTerm (annotationToTerm annot)


Definition lists
----------------

> definitionToRels :: Language -> WiktionaryTerm -> AnnotatedString -> [WiktionaryRel]
> definitionToRels language thisTerm definition =
>   [makeRel "definition" thisTerm (simpleTerm language (A.unannotate definition))]
>   ++ (map (annotationToRel thisTerm) (A.annotations definition))


Looking up sections
-------------------

> findHeading :: [ByteString] -> [ByteString] -> Maybe ByteString
> findHeading choices headings =
>   let intersection = intersect choices headings in maybeHead intersection
>
> findPrefixedHeading :: ByteString -> [ByteString] -> Maybe ByteString
> findPrefixedHeading prefix headings =
>   let {
>     filtered = filter (Char8.isPrefixOf prefix) headings;
>     mapped   = map (Char8.drop (Char8.length prefix)) filtered
>   } in maybeHead mapped
>
> maybeHead :: [a] -> Maybe a
> maybeHead list = if (length list) > 0 then Just (head list) else Nothing

