> {-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
> module Text.MediaWiki.AnnotatedString where
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.UTF8 as UTF8
> import Text.MediaWiki.AList (ByteAssoc, get, filterEmpty)
> import Data.ByteString (ByteString)
> import Data.String (IsString, fromString)
> import Prelude hiding (append, concat, unlines)


Text can be marked up with things such as internal links. During parsing,
we will want to keep track of the annotations on text, without having to
use a full-blown AST to represent which spans the annotations applied to.

Annotations can represent MediaWiki links, which have a `namespace`, `page`,
and `section`, or more complex relationships expressed by templates. We
represent these all using an association list (a list of tuples).

> type Annotation = [ByteAssoc]


`makeLink` is a constant that can be used as a template for making Annotations
for internal links.

> makeLink :: ByteString -> ByteString -> ByteString -> Annotation
> makeLink namespace page section = filterEmpty [
>   ("namespace", namespace),
>   ("page", page),
>   ("section", section)]

The simplifying assumption here is that, in a parse rule that produces
annotations, the annotations apply to the entire span of text that was parsed.
So what we need to keep track of in AnnotatedString is one string (as a
ByteString) and a list of Annotations for it.

> data AnnotatedString = AnnotatedString [Annotation] ByteString deriving (Show, Eq)
>
> annotate :: [Annotation] -> ByteString -> AnnotatedString
> annotate annos t = AnnotatedString annos t
>
> annotations :: AnnotatedString -> [Annotation]
> annotations (AnnotatedString annos t) = annos
>
> fromBytes :: ByteString -> AnnotatedString
> fromBytes = annotate []
>
> empty :: AnnotatedString
> empty = fromBytes ""
>
> append :: AnnotatedString -> AnnotatedString -> AnnotatedString
> append (AnnotatedString a1 t1) (AnnotatedString a2 t2)
>   = AnnotatedString (a1 ++ a2) (BS.append t1 t2)
>
> appendSep :: ByteString -> AnnotatedString -> AnnotatedString -> AnnotatedString
> appendSep sep (AnnotatedString a1 t1) (AnnotatedString a2 t2)
>   = AnnotatedString (a1 ++ a2) (BS.append (BS.append t1 sep) t2)
>
> concat :: [AnnotatedString] -> AnnotatedString
> concat = foldl append empty
>
> join :: ByteString -> [AnnotatedString] -> AnnotatedString
> join sep [] = empty
> join sep ats = foldl1 (appendSep sep) ats
>
> unlines :: [AnnotatedString] -> AnnotatedString
> unlines ats = append (join "\n" ats) (fromBytes "\n")
>
> unannotate :: AnnotatedString -> ByteString
> unannotate (AnnotatedString a t) = t
>
> transformA :: (ByteString -> ByteString) -> AnnotatedString -> AnnotatedString
> transformA op (AnnotatedString a t) = AnnotatedString a (op t)

We can use a string literal as an AnnotatedString:

> instance IsString AnnotatedString where
>   fromString = (fromBytes . UTF8.fromString)
