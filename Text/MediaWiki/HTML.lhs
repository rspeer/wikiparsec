> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

`Text.MediaWiki.HTML`: removing HTML from Wikitext
==================================================

After parsing the XML of a MediaWiki entry, the result then needs to be
decoded as (potentially very sloppy) HTML, the contents of which are Wikitext.
This module is responsible for handling the HTML and converting it into a string
of Wikitext without HTML tags.

Here is an example snippet of Wikitext that includes HTML:

```html
  Albedo <math>{\alpha}</math> can then be given as:

  :<math>{\alpha}= (1-D) \bar \alpha(\theta_i) + D \bar{ \bar \alpha}.</math>

  [[Directional-hemispherical reflectance]] is sometimes referred to as
  <em>black-sky albedo</em> and [[bi-hemispherical reflectance]] as
  <em>white-sky albedo</em>.  These terms are important because they allow the
  albedo to be calculated for any given illumination conditions from a
  knowledge of the intrinsic properties of the surface.<ref
  name="BlueskyAlbedo"/>
```

We want to remove the tags, leaving just the text. In the case of the `<math>` tag,
we want to remove its text content as well, so we don't end up with LaTeX in our
output. The desired output is this string of Wikitext:

```
  Albedo  can then be given as:

  :

  [[Directional-hemispherical reflectance]] is sometimes referred to as
  black-sky albedo and [[bi-hemispherical reflectance]] as white-sky albedo.
  These terms are important because they allow the albedo to be calculated for
  any given illumination conditions from a knowledge of the intrinsic
  properties of the surface.
```

Handling HTML with TagSoup
--------------------------

We import a bunch of stuff from the `TagSoup` library. In particular, we
import `parseTags` from its `Fast.Utf8Only` variant, because our input is UTF-8
only and we like being fast.

TagSoup's `parseTags` takes in HTML and returns a lazy list of tag-related
events, making it much like SAX for XML.

> module Text.MediaWiki.HTML where
> import WikiPrelude
> import Text.HTML.TagSoup hiding (parseTags, renderTags)
> import Text.HTML.TagSoup.Fast.Utf8Only

We have a ByteString of HTML. We're going to leave it as a ByteString for a while,
because `TagSoup.Fast.Utf8Only` works on ByteStrings of UTF-8.

We run `parseTags` on the HTML to get a list of tag events, then run our
function `extractFromTags`, which will give us the text contained within the
tags as ByteString values. We concatenate those ByteStrings together, and
finally decode the result into Text at the end.

> extractWikiTextFromHTML :: ByteString -> Text
> extractWikiTextFromHTML = decodeUtf8 . mconcat . extractFromTags . parseTags

The `Tag` type is an enumeration of possible tag events: `TagOpen`, `TagClose`,
`TagText`, and possibly other events we don't care about. `extractFromTags`
handles these different cases with different implementations.

> extractFromTags :: [Tag ByteString] -> [ByteString]

Our main goal is to ignore tags and get at the text. When we see a `TagOpen`,
we don't pay much attention to it. But there are some tags we want to ignore
extra hard, identified by the `skippedSpan` function. More on that below.

We avoid skipping in a particular case where the HTML was misparsed, identified
by `(slashedAttrs attrs)`, because typically this would just consume the rest of
the page looking for a closing tag that won't exist.

> extractFromTags ((TagOpen tag attrs):rest) =
>   if (skippedSpan tag && not (slashedAttrs attrs))
>     then skipUntilClose tag rest
>     else extractFromTags rest

When we encounter `TagText`, we cons its content onto our output list.

> extractFromTags ((TagText text):rest) = text:(extractFromTags rest)

When we encounter any other event, we ignore it.

> extractFromTags (_:rest) = extractFromTags rest

When there are no tags remaining, there is nothing left to extract.

> extractFromTags [] = []

Skipping obnoxious tags
-----------------------

There are some tags we'll want to skip entirely, not trying to extract any text
from their contents. The `<code>` tag is a straightforward example. Some of
these tags are made up by MediaWiki, instead of being typical HTML tags.

When we see one of these tags, we go into a different recursive method,
`skipUntilClose`, that doesn't extract any text. It just barrels through the
events, throwing most of them out, until it encounters the corresponding
closing tag, at which point it returns control to `extractFromTags`.

This would give slightly erroneous results if these tags were ever nested
recursively, because we only keep track of a single closing tag we're looking
for, not a stack.  There is never a reason to nest these tags within
themselves, though, and doing so would probably confuse MediaWiki as much as it
confuses us.

> skippedSpan :: ByteString -> Bool
> skippedSpan tag = tag == "math" || tag == "code" || tag == "ref" ||
>                   tag == "gallery" || tag == "hiero" || tag == "timeline"
>
> skipUntilClose :: ByteString -> [Tag ByteString] -> [ByteString]
> skipUntilClose target ((TagClose tag):rest) =
>   if tag == target
>     then extractFromTags rest
>     else skipUntilClose target rest
> skipUntilClose target (_:rest) = skipUntilClose target rest
> skipUntilClose target [] = []

Self-closing tags, like `<ref name="reference" />`, are a bit weird. They are
intended to show up as a `TagOpen` immediately followed by a `TagClose`, which
`skipUntilClose` would handle perfectly well.  However, MediaWiki and TagSoup
disagree on whether the self-closing slash needs a space before it. If the
space isn't there, the tag may end up parsed as an opening tag with one of its
attribute names ending in `/`, and that would make `skipUntilClose` skip the
rest of the entire page.

`slashedAttrs` identifies when that has happened and lets us just ignore the
tag.

> slashedAttrs :: [(ByteString,ByteString)] -> Bool
> slashedAttrs ((name,value):rest) = slashed name || slashed value || slashedAttrs rest
> slashedAttrs [] = False
> slashed = isSuffixOf "/"

