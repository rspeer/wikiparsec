> {-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
> module Text.MediaWiki.AnnotatedText where
> import qualified Data.Text as T
> import Data.Text (Text)
> import Prelude hiding (append, concat, unlines)

Text can be marked up with things such as internal links. During parsing,
we will want to keep track of the annotations on text, without having to
use a full-blown AST to represent which spans the annotations applied to.

Annotations can represent MediaWiki links, which have a `namespace`, `page`,
and `section`, or more complex relationships expressed by templates. To avoid
proliferation of abstractions, we keep them all in the same type, using the
empty string for components that are absent or do not apply.

> data Annotation = Annotation {
>   rel :: Text,
>   namespace :: Text,
>   page :: Text,
>   section :: Text
> } deriving (Show, Eq)

`makeLink` is a constant that can be used as a template for making Annotations
for internal links.

> makeLink :: Annotation
> makeLink = Annotation {rel="Link", namespace="", page="", section=""}

The simplifying assumption here is that, in a parse rule that produces
annotations, the annotations apply to the entire span of text that was parsed.
So what we need to keep track of in AnnotatedText is one string of Text and a
list of Annotations for it.

> data AnnotatedText = AnnotatedText [Annotation] Text deriving (Show, Eq)
>
> annotate :: [Annotation] -> Text -> AnnotatedText
> annotate annos t = AnnotatedText annos t
>
> fromText :: Text -> AnnotatedText
> fromText = annotate []
>
> empty :: AnnotatedText
> empty = fromText ""
>
> append :: AnnotatedText -> AnnotatedText -> AnnotatedText
> append (AnnotatedText a1 t1) (AnnotatedText a2 t2)
>   = AnnotatedText (a1 ++ a2) (T.append t1 t2)
>
> appendSep :: Text -> AnnotatedText -> AnnotatedText -> AnnotatedText
> appendSep sep (AnnotatedText a1 t1) (AnnotatedText a2 t2)
>   = AnnotatedText (a1 ++ a2) (T.append (T.append t1 sep) t2)
>
> concat :: [AnnotatedText] -> AnnotatedText
> concat = foldl append empty
>
> join :: Text -> [AnnotatedText] -> AnnotatedText
> join sep [] = empty
> join sep ats = foldl1 (appendSep sep) ats
>
> unlines :: [AnnotatedText] -> AnnotatedText
> unlines ats = append (join "\n" ats) (fromText "\n")
>
> unannotate :: AnnotatedText -> Text
> unannotate (AnnotatedText a t) = t

