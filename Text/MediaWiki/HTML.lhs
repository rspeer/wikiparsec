> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

After parsing the XML of a MediaWiki entry, the result then needs to be
decoded as (potentially very sloppy) HTML, the contents of which are Wikitext.
This module is responsible for handling the HTML.

> module Text.MediaWiki.HTML where
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as Char8
> import Text.HTML.TagSoup hiding (parseTags, renderTags)
> import Text.HTML.TagSoup.Fast.Utf8Only

> extractWikiTextFromHTML :: ByteString -> ByteString
> extractWikiTextFromHTML = Char8.concat . extractFromTags . parseTags

> skippedSpan :: ByteString -> Bool
> skippedSpan tag = tag == "math" || tag == "code" || tag == "ref" ||
>                   tag == "gallery" || tag == "hiero" || tag == "timeline"
>
> slashedAttrs :: [(ByteString,ByteString)] -> Bool
> slashedAttrs ((name,value):rest) = slashed name || slashed value || slashedAttrs rest
> slashedAttrs [] = False
> slashed = Char8.isSuffixOf "/"
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


