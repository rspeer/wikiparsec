Some string-splitting functions that help us parse Wikitext without
backtracking:

> module Text.MediaWiki.SplitUtils (splitFirst, splitLast, strictReplace) where
> import qualified Data.ByteString.Search as BSS
> import qualified Data.ByteString.Lazy as BSL
> import qualified Data.ByteString as BS
> import Data.ByteString (ByteString)

`splitFirst` finds the first occurrence of a separator character, and
splits the string at that point, returning the text before and after the
separator. If the separator never appears, the suffix will be the empty string.

> splitFirst :: ByteString -> ByteString -> (ByteString, ByteString)
> splitFirst = BSS.breakOn

`splitLast` does the same with the *last* occurrence, and it's easier to define
as a reversal of `splitFirst`.

> splitLast :: ByteString -> ByteString -> (ByteString, ByteString)
> splitLast delim bytes =
>   let (beforeR, afterR) = BSS.breakOn (BS.reverse delim) (BS.reverse bytes) in
>     (BS.reverse afterR, BS.reverse beforeR)

> strictReplace :: ByteString -> ByteString -> ByteString -> ByteString
> strictReplace pat sub text = BSL.toStrict $ BSS.replace pat sub text
