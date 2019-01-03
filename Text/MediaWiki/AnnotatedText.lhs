`Text.MediaWiki.AnnotatedText`: a data type for slightly-marked-up text
=======================================================================

> {-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
> module Text.MediaWiki.AnnotatedText where
> import WikiPrelude

Text can be marked up with things such as internal links. During parsing,
we will want to keep track of the annotations on text, without having to
use a full-blown AST to represent which spans the annotations applied to.

Annotations can represent MediaWiki links, which have a `namespace`, `page`,
and `section`, or more complex relationships expressed by templates. We
represent these using a Map.

> type Annotation = Map Text Text

`annotationFromList` builds an Annotation, much like `mapFromList`, but also
avoids putting unnecessary empty values into the map.

> annotationFromList :: [(Text, Text)] -> Annotation
> annotationFromList pairs = mapFromList (filter filterEmpty pairs)
>
> filterEmpty :: (Text, Text) -> Bool
> filterEmpty (a, b) = b /= ""

The simplifying assumption here is that, in a parse rule that produces
annotations, the annotations apply to the entire span of text that was parsed.
So what we need to keep track of in an AnnotatedText is one string (as a
Text) and a list of Annotations for it.

> data AnnotatedText = AnnotatedText [Annotation] Text deriving (Show, Eq)
>
> annotate :: [Annotation] -> Text -> AnnotatedText
> annotate annos t = AnnotatedText annos t

Some simple functions to extract values from AnnotatedText:

> getAnnotations :: AnnotatedText -> [Annotation]
> getAnnotations (AnnotatedText annos t) = annos
>
> getText :: AnnotatedText -> Text
> getText (AnnotatedText annos t) = t

Links
-----

`makeLink` is a constant that can be used as a template for making Annotations
for internal links.

> makeLink :: Text -> Text -> Text -> Annotation
> makeLink namespace page section = annotationFromList [
>   ("rel", "link"),
>   ("namespace", namespace),
>   ("page", page),
>   ("section", section)]

`filterLink` tests whether an annotation is a link.

> filterLink :: Annotation -> Bool
> filterLink annot = (get "rel" annot) == "link"
>
> filterArticleLink :: Annotation -> Bool
> filterArticleLink annot = (filterLink annot) && (get "namespace" annot) == ""

If links in particular are what we're interested in, we can use `getLinks` or
`getArticleLinks`.

> getLinks :: AnnotatedText -> [Annotation]
> getLinks atext = filter filterLink (getAnnotations atext)
>
> getArticleLinks :: AnnotatedText -> [Text]
> getArticleLinks atext = map (get "page") (filter filterArticleLink (getAnnotations atext))

Operations on AnnotatedTexts
----------------------------

We convert plain Text into AnnotatedText by annotating it with nothing.
The same goes for a plain ByteString, except we have to decode it from UTF-8
first.

> annotFromText :: Text -> AnnotatedText
> annotFromText = annotate []
>
> annotFromBytes :: ByteString -> AnnotatedText
> annotFromBytes = annotFromText . decodeUtf8

`singleAnnotation` maps a single key to a single value, with no text. This can
be concatenated to other AnnotatedText as a way to add a property.

> singleAnnotation :: Text -> Text -> AnnotatedText
> singleAnnotation key val = annotate [singletonMap key val] ""

An AnnotatedText is a Monoid, meaning that it can be concatenated:

> instance Semigroup AnnotatedText where
>   (AnnotatedText a1 t1) <> (AnnotatedText a2 t2)
>     = AnnotatedText (a1 ++ a2) (t1 ++ t2)

> instance Monoid AnnotatedText where
>   mempty  = annotFromText ""

One particular kind of concatenation we'll want to do is joining
AnnotatedTexts with line breaks between the texts:

> joinAnnotatedLines :: [AnnotatedText] -> AnnotatedText
> joinAnnotatedLines ats =
>   annotate (mconcat (map getAnnotations ats))
>            (unlines (map getText ats))
>
> transformA :: (Text -> Text) -> AnnotatedText -> AnnotatedText
> transformA op (AnnotatedText a t) = AnnotatedText a (op t)

We can use a string literal as an AnnotatedText. `pack` converts Haskell's
shitty built-in `String` type to `Text`, and `annotFromText` converts that
to an `AnnotatedText`.

> instance IsString AnnotatedText where
>   fromString = (annotFromText . pack)
