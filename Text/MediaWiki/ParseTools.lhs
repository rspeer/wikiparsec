> {-# LANGUAGE RankNTypes, NoMonomorphismRestriction, OverloadedStrings #-}

> module Text.MediaWiki.ParseTools where
> import qualified Data.Text as T
> import qualified Text.MediaWiki.AnnotatedText as A
> import Text.MediaWiki.AnnotatedText (AnnotatedText)
> import Prelude hiding (takeWhile)
> import Data.Text (Text)
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator
> import Control.Applicative ((<|>), (<$>), pure, empty)


Sensible names for things
=========================

Let's rename this function that appends a character to the end of a text,
for people who don't keep a copy of SICP under their pillow:

> appendChar = T.snoc

As part of many expressions, we need a quick way to discard what we matched
and use the empty text as its value:

> nop :: Parser Text
> nop = return ""

Common parsing functions
========================

A lot of spans of Wikitext are mostly defined by what they're not. The
`textWithout` rule matches and returns a sequence of 1 or more characters that
are not in the given string.

> takeTill1 :: (Char -> Bool) -> Parser Text
> takeTill1 pred = do
>   c    <- satisfy (not . pred)
>   rest <- takeTill pred
>   return (T.cons c rest)
>
> textWithout :: [Char] -> Parser Text
> textWithout chars = takeTill1 (inClass chars)
>
> textWith :: [Char] -> Parser Text
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

> textChoices :: [Parser Text] -> Parser Text
> textChoices options = concatMany (choice options)
>
> concatMany :: Parser Text -> Parser Text
> concatMany combinator = do
>   parts <- many1 combinator
>   return (T.concat parts)

Most of the expressions we write will match at least one character, allowing
us to repeat them without allowing repeated matches of the empty string.
However, there are cases where the empty string is a valid value for a
sub-expression. In those cases, we wrap the sub-expression in `possiblyEmpty`.

> possiblyEmpty :: Parser Text -> Parser Text
> possiblyEmpty combinator = option "" combinator

Sometimes a token starts some special environment that will consume everything
until an ending token. An example would be HTML comments, which consume
everything between `<!--` and `-->`.

We need to output something besides an error in the case where the ending token
never appears, though. What we choose to do is to consume everything up to the
end of the input, and return what we consumed.

> delimitedSpan :: Text -> Text -> Parser Text
> delimitedSpan open close = do
>   string open
>   chars <- manyTill anyChar (string close <|> (endOfInput >> nop))
>   return (T.pack chars)

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

Expressions for AnnotatedText
=============================

AnnotatedText versions of some of the operators above:

> aTextChoices :: [Parser AnnotatedText] -> Parser AnnotatedText
> aTextChoices options = aConcatMany (choice options)
>
> aConcatMany :: Parser AnnotatedText -> Parser AnnotatedText
> aConcatMany combinator = do
>   parts <- many1 combinator
>   return (A.concat parts)
>
> aPossiblyEmpty :: Parser AnnotatedText -> Parser AnnotatedText
> aPossiblyEmpty combinator = option A.empty combinator

