`Text.SplitUtils`: manipulating delimited strings
=================================================

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

Some string-splitting functions will help us parse Wikitext without
backtracking:

> module Text.SplitUtils (splitFirst, splitLast) where
> import WikiPrelude

`splitFirst` finds the first occurrence of a separator character, and
splits the string at that point, returning the text before and after the
separator. If the separator never appears, the suffix will be the empty string.

It turns out that this exact function is in the Classy Prelude, and it's called
`breakOn`.

> splitFirst :: Text -> Text -> (Text, Text)
> splitFirst = breakOn

`splitLast` does the same with the *last* occurrence, and it's easier to define
as a reversal of `splitFirst`.

> splitLast :: Text -> Text -> (Text, Text)
> splitLast delim text =
>   let (beforeR, afterR) = breakOn (reverse delim) (reverse text) in
>     (reverse afterR, reverse beforeR)
