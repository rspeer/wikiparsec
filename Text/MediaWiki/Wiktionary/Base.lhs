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
> import qualified Data.ByteString.Char8 as Char8
> import Control.Applicative ((<|>), (<$>), (*>), (<*))

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
>   sense :: Maybe ByteString
> } deriving (Show, Eq)
>
> simpleTerm language text = WiktionaryTerm {
>   text=text, language=Just language, sense=Nothing
>   }

A WiktionaryRel expresses a relationship between terms that we can extract
from a page.

> data WiktionaryRel = WiktionaryRel {
>   relation :: ByteString,
>   fromTerm :: WiktionaryTerm,
>   toTerm :: WiktionaryTerm
> }
>
> makeRel :: ByteString -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryRel
> makeRel rel from to = WiktionaryRel { relation=rel, fromTerm=from, toTerm=to }
> makeGenericRel = makeRel "RelatedTo"


Converting an Annotation representing a term to a WiktionaryTerm:

> annotationToTerm :: Annotation -> WiktionaryTerm
> annotationToTerm annot = WiktionaryTerm {
>   text=(get "page" annot),
>   language=(lookupMaybe ["language", "section"] annot),
>   sense=(lookup "sense" annot)
>   }
>
> annotationToRel :: WiktionaryTerm -> Annotation -> [WiktionaryRel]
> annotationToRel thisTerm annot =
>   makeRel (getDefault "link" "rel" annot) thisTerm (annotationToTerm annot)


Definition lists
----------------

> definitionList :: Parser [AnnotatedString]
> definitionList = do
>   listItems <- (orderedList "#")
>   return (extractTopLevel listItems)
>
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
>     filtered = filter (isPrefixOf prefix) headings;
>     mapped   = map (Char8.drop (Char8.length prefix)) filtered
>   } in maybeHead mapped
>
> maybeHead :: [a] -> Maybe a
> maybeHead list = if (length list) > 0 then Just (head list) else Nothing

