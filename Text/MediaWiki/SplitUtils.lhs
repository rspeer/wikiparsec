Some string-splitting functions that help us parse Wikitext without
backtracking:

> module Text.MediaWiki.SplitUtils (splitFirst, splitLast, tSplitFirst, tSplitLast) where
> import qualified Data.Text as T
> import Data.Text (Text)

`splitFirst` finds the first occurrence of a separator character, and
splits the string at that point, returning the text before and after the
separator. If the separator never appears, the suffix will be the empty string.

> splitFirst :: (Eq a) => a -> [a] -> ([a], [a])
> splitFirst char [] = ([], [])
> splitFirst char (next:rest) =
>   if char == next
>   then ([], rest)
>   else let (before, after) = splitFirst char rest
>         in (next:before, after)

`splitLast` finds the *last* occurrence of a separator character
and splits the string at that point. This is easiest to define as a reversal
of `splitFirst`.

> splitLast :: (Eq a) => a -> [a] -> ([a], [a])
> splitLast char str =
>   let (beforeR, afterR) = splitFirst char (reverse str) in
>     (reverse afterR, reverse beforeR)

`tSplitFirst` and `tSplitLast` are versions of these that operate on Texts
instead of Strings.

> tSplitFirst :: Char -> Text -> (Text, Text)
> tSplitFirst char t =
>   let (before, after) = splitFirst char (T.unpack t)
>   in  (T.pack before, T.pack after)

> tSplitLast :: Char -> Text -> (Text, Text)
> tSplitLast char t =
>   let (before, after) = splitLast char (T.unpack t)
>   in  (T.pack before, T.pack after)
