> {-# LANGUAGE RankNTypes, NoMonomorphismRestriction, OverloadedStrings #-}

> module Text.MediaWiki.ParseTools where
> import qualified Text.MediaWiki.AnnotatedString as A
> import qualified Data.ByteString.Char8 as Char8
> import qualified Data.ByteString.Char8 as Char8
> import Data.ByteString (ByteString)
> import Text.MediaWiki.AnnotatedString (AnnotatedString)
> import Prelude hiding (takeWhile)
> import Data.Attoparsec.ByteString.Char8
> import Data.Attoparsec.Combinator
> import Control.Applicative ((<|>), (<$>), pure, empty)


Sensible names for things
=========================

Let's rename the functions that add a byte to the start or end of a
ByteString, for people who don't keep a copy of SICP under their pillow:

> prependChar = Char8.cons
> appendChar = Char8.snoc

As part of many expressions, we need a quick way to discard what we matched
and use the empty string as its value:

> nop :: Parser ByteString
> nop = return ""


Common parsing functions
========================

A lot of spans of Wikitext are mostly defined by what they're not. The
`textWithout` rule matches and returns a sequence of 1 or more characters that
are not in the given string.

> takeTill1 :: (Char -> Bool) -> Parser ByteString
> takeTill1 pred = do
>   c    <- satisfy (not . pred)
>   rest <- takeTill pred
>   return (Char8.cons c rest)
>
> textWithout :: [Char] -> Parser ByteString
> textWithout chars = takeTill1 (inClass chars)
>
> textWith :: [Char] -> Parser ByteString
> textWith chars = takeWhile1 (inClass chars)
>
> skipChars :: [Char] -> Parser ()
> skipChars chars = skipWhile (inClass chars)

Sometimes there are many different kinds of strings that could appear in a
given context, including different kinds of strings concatenated together. For
example, in one context, we might accept plain text, links, and templates, but
not line breaks.

To make this easier, we'll define `textChoices`, which takes a list of
expressions we're allowed to parse, tries all of them in that priority order,
and concatenates together their results.

> textChoices :: [Parser ByteString] -> Parser ByteString
> textChoices options = concatMany (choice options)
>
> concatMany :: Parser ByteString -> Parser ByteString
> concatMany combinator = do
>   parts <- many1 combinator
>   return (Char8.concat parts)

Most of the expressions we write will match at least one character, allowing
us to repeat them without allowing repeated matches of the empty string.
However, there are cases where the empty string is a valid value for a
sub-expression. In those cases, we wrap the sub-expression in `possiblyEmpty`.

> possiblyEmpty :: Parser ByteString -> Parser ByteString
> possiblyEmpty combinator = option "" combinator

Sometimes a token starts some special environment that will consume everything
until an ending token. An example would be HTML comments, which consume
everything between `<!--` and `-->`.

We need to output something besides an error in the case where the ending token
never appears, though. What we choose to do is to consume everything up to the
end of the input, and return what we consumed.

> delimitedSpan :: ByteString -> ByteString -> Parser ByteString
> delimitedSpan open close = do
>   string open
>   chars <- manyTill anyChar (string close <|> (endOfInput >> nop))
>   return (Char8.pack chars)

A limited version of Parsec's `notFollowedBy`:

> notFollowedByChar :: Char -> Parser ()
> notFollowedByChar c = do
>   maybeChar <- peekChar
>   case maybeChar of
>     Nothing -> return ()
>     Just c' -> if (c == c') then empty else return ()

Another function missing in Attoparsec:

> optionMaybe :: Parser a -> Parser (Maybe a)
> optionMaybe p = Just <$> p <|> pure Nothing

A function that turns a parser into a pure function:

> parseOrDefault :: a -> Parser a -> ByteString -> a
> parseOrDefault def parser input =
>   case parseOnly parser input of
>     Left err -> def
>     Right x  -> x

Expressions for AnnotatedStrings
================================

AnnotatedString versions of some of the operators above:

> aTextChoices :: [Parser AnnotatedString] -> Parser AnnotatedString
> aTextChoices options = aConcatMany (choice options)
>
> aConcatMany :: Parser AnnotatedString -> Parser AnnotatedString
> aConcatMany combinator = do
>   parts <- many1 combinator
>   return (A.concat parts)
>
> aPossiblyEmpty :: Parser AnnotatedString -> Parser AnnotatedString
> aPossiblyEmpty combinator = option A.empty combinator


