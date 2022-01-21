`Text.SplitUtils`: manipulating delimited strings
=================================================

Some string-splitting functions will help us parse Wikitext without
backtracking:

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
> module Text.SplitUtils (splitFirst, splitLast, removeParentheticals) where
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

Removes all text that is inside parentheses, where parentheses are matched like you
would expect. (This is better than the obvious regex, which would simply remove from
the first "(" to the first ")", leaving residue when parentheticals are nested.)

Also removes a single space before the opening parenthesis, if it's there.

> removeParentheticals :: Text -> Text
> removeParentheticals = pack . removeParensString . unpack
>
> removeParensString :: String -> String
> removeParensString = bal 0
>
> bal :: Int -> String -> String
> bal _ [] = []
> bal n (' ':'(':tail) = bal (n + 1) tail
> bal n ('(':tail) = bal (n + 1) tail
> bal 0 (')':tail) = bal 0 tail
> bal n (')':tail) = bal (n - 1) tail
> bal 0 (c:tail) = c:(bal 0 tail)
> bal n (c:tail) = bal n tail
