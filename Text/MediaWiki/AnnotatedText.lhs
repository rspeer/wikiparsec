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

`makeLink` is a constant that can be used as a template for making Annotations
for internal links.

> makeLink :: Text -> Text -> Text -> Annotation
> makeLink namespace page section = mapFromList [
>   ("namespace", namespace),
>   ("page", page),
>   ("section", section)]

The simplifying assumption here is that, in a parse rule that produces
annotations, the annotations apply to the entire span of text that was parsed.
So what we need to keep track of in an AnnotatedText is one string (as a
Text) and a list of Annotations for it.

> data AnnotatedText = AnnotatedText [Annotation] Text deriving (Show, Eq)
>
> annotate :: [Annotation] -> Text -> AnnotatedText
> annotate annos t = AnnotatedText annos t
>
> getAnnotations :: AnnotatedText -> [Annotation]
> getAnnotations (AnnotatedText annos t) = annos
>
> getText :: AnnotatedText -> Text
> getText (AnnotatedText annos t) = t
>
> annotFromText :: Text -> AnnotatedText
> annotFromText = annotate []
>
> singleAnnotation :: Annotation -> AnnotatedText
> singleAnnotation annot = annotate [annot] ""

An AnnotatedText is Joinable, meaning it has an empty value and can be
concatenated:

> instance Joinable AnnotatedText where
>   mempty  = annotFromText ""
>   mappend (AnnotatedText a1 t1) (AnnotatedText a2 t2)
>     = AnnotatedText (a1 ++ a2) (t1 ++ t2)

One particular kind of concatenation we'll want to do is joining
AnnotatedTexts with line breaks between the texts:

> joinAnnotatedLines :: [AnnotatedText] -> AnnotatedText
> joinAnnotatedLines ats =
>   annotate (mconcat (map getAnnotations ats))
>            (unlines (map getText ats))
>
> transformA :: (Text -> Text) -> AnnotatedText -> AnnotatedText
> transformA op (AnnotatedText a t) = AnnotatedText a (op t)

We can use a string literal as an AnnotatedText:

> instance IsString AnnotatedText where
>   fromString = (annotFromText . pack)
