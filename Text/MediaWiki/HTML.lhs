> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

After parsing the XML of a MediaWiki entry, the result then needs to be
decoded as (potentially very sloppy) HTML, the contents of which are Wikitext.
This module is responsible for handling the HTML.

> module Text.MediaWiki.HTML where
> import Data.Text (Text)
> import qualified Data.Text as T
> import Text.HTML.TagSoup

> extractWikiTextFromHTML :: Text -> Text
> extractWikiTextFromHTML html = T.concat (extractFromTags (parseTagsOptions parseOptionsFast html))

> skippedSpan :: Text -> Bool
> skippedSpan tag = tag == "math" || tag == "code" || tag == "ref" ||
>                   tag == "gallery" || tag == "hiero" || tag == "timeline"
>
> slashedAttrs :: [(Text,Text)] -> Bool
> slashedAttrs ((name,value):rest) = slashed name || slashed value || slashedAttrs rest
> slashedAttrs [] = False
> slashed = T.isSuffixOf "/"
>
> extractFromTags :: [Tag Text] -> [Text]
> extractFromTags ((TagOpen tag attrs):rest) =
>   if (skippedSpan tag && not (slashedAttrs attrs))
>     then skipUntilClose tag rest
>     else extractFromTags rest
> extractFromTags ((TagText text):rest) = text:(extractFromTags rest)
> extractFromTags (_:rest) = extractFromTags rest
> extractFromTags [] = []
>
> skipUntilClose :: Text -> [Tag Text] -> [Text]
> skipUntilClose target ((TagClose tag):rest) =
>   if tag == target
>     then extractFromTags rest
>     else skipUntilClose target rest
> skipUntilClose target (_:rest) = skipUntilClose target rest
> skipUntilClose target [] = []


