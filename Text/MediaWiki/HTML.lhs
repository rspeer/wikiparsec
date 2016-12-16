> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

After parsing the XML of a MediaWiki entry, the result then needs to be
decoded as (potentially very sloppy) HTML, the contents of which are Wikitext.
This module is responsible for handling the HTML.

We import a bunch of stuff from the `TagSoup` library. In particular, we
import `parseTags` from its `Fast.Utf8Only` variant, because our input is UTF-8
only and we like being fast.

> module Text.MediaWiki.HTML where
> import WikiPrelude
> import Text.HTML.TagSoup hiding (parseTags, renderTags)
> import Text.HTML.TagSoup.Fast.Utf8Only

We have a ByteString of HTML. We're going to leave it as a ByteString for a while,
because `TagSoup.Fast.Utf8Only` works on ByteStrings of UTF-8.

We run `parseTags` on the HTML to get a list of tags, then run our function
`extractFromTags`, which will give us the ByteString contents of the tags
we're interested in. We concatenate those ByteStrings together, and finally
decode the result into Text at the end.

> extractWikiTextFromHTML :: ByteString -> Text
> extractWikiTextFromHTML = decodeUtf8 . mconcat . extractFromTags . parseTags

> skippedSpan :: ByteString -> Bool
> skippedSpan tag = tag == "math" || tag == "code" || tag == "ref" ||
>                   tag == "gallery" || tag == "hiero" || tag == "timeline"
>
> extractFromTags :: [Tag ByteString] -> [ByteString]
> extractFromTags ((TagOpen tag attrs):rest) =
>   if (skippedSpan tag && not (slashedAttrs attrs))
>     then skipUntilClose tag rest
>     else extractFromTags rest
> extractFromTags ((TagText text):rest) = text:(extractFromTags rest)
> extractFromTags (_:rest) = extractFromTags rest
> extractFromTags [] = []
>
> skipUntilClose :: ByteString -> [Tag ByteString] -> [ByteString]
> skipUntilClose target ((TagClose tag):rest) =
>   if tag == target
>     then extractFromTags rest
>     else skipUntilClose target rest
> skipUntilClose target (_:rest) = skipUntilClose target rest
> skipUntilClose target [] = []
>
> slashedAttrs :: [(ByteString,ByteString)] -> Bool
> slashedAttrs ((name,value):rest) = slashed name || slashed value || slashedAttrs rest
> slashedAttrs [] = False
> slashed = isSuffixOf "/"


