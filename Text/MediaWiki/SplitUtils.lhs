> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Some string-splitting functions that help us parse Wikitext without
backtracking:

> module Text.MediaWiki.SplitUtils (splitFirst, splitLast) where
> import WikiPrelude
> import Data.Text (breakOn)

`splitFirst` finds the first occurrence of a separator character, and
splits the string at that point, returning the text before and after the
separator. If the separator never appears, the suffix will be the empty string.

> splitFirst :: Text -> Text -> (Text, Text)
> splitFirst = breakOn

`splitLast` does the same with the *last* occurrence, and it's easier to define
as a reversal of `splitFirst`.

> splitLast :: Text -> Text -> (Text, Text)
> splitLast delim text =
>   let (beforeR, afterR) = breakOn (reverse delim) (reverse text) in
>     (reverse afterR, reverse beforeR)
