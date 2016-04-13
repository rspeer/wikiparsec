> {-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}

> module Text.MediaWiki.ParseTools where
> import WikiPrelude hiding (takeWhile)
> import Text.MediaWiki.AnnotatedText
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator


Sensible names for things
=========================

Let's rename the functions that add a character to the start or end of a
sequence, for people who don't keep a copy of SICP under their pillow:

> prependChar :: (Joinable a) => Char -> Text -> Text
> prependChar = cons
>
> appendChar :: Text -> Char -> Text
> appendChar = snoc

As part of many expressions, we need a quick way to discard what we matched
and return nothing, such as a Text parser that returns the empty text:

> nop :: (Joinable a) => Parser a
> nop = return mempty


Common parsing functions
========================

A lot of spans of Wikitext are mostly defined by what they're not. The
`textWithout` rule matches and returns a sequence of 1 or more characters that
are not in the given string.

> takeTill1 :: (Char -> Bool) -> Parser Text
> takeTill1 pred = do
>   c    <- satisfy (not . pred)
>   rest <- takeTill pred
>   return (cons c rest)
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

> optionalTextChoices :: (Joinable a) => [Parser a] -> Parser a
> optionalTextChoices options = concatMany (choice options)
>
> textChoices :: (Joinable a) => [Parser a] -> Parser a
> textChoices options = concatMany1 (choice options)
>
> concatMany :: (Joinable a) => Parser a -> Parser a
> concatMany combinator = do
>   parts <- many' combinator
>   return (concat parts)
>
> concatMany1 :: (Joinable a) => Parser a -> Parser a
> concatMany1 combinator = do
>   parts <- many1 combinator
>   return (concat parts)

Most of the expressions we write will match at least one character, allowing
us to repeat them without allowing repeated matches of the empty string.
However, there are cases where the empty string is a valid value for a
sub-expression. In those cases, we wrap the sub-expression in `possiblyEmpty`.

> possiblyEmpty :: (Joinable a) => Parser a -> Parser a
> possiblyEmpty combinator = option mempty combinator

A limited version of Parsec's `notFollowedBy`:

> notFollowedByChar :: Char -> Parser ()
> notFollowedByChar c = do
>   maybeChar <- peekChar
>   case maybeChar of
>     Nothing -> return ()
>     Just c' -> if (c == c') then empty else return ()

Another function missing in Attoparsec. `optionMaybe` takes a parser for `a`
that might fail, and turns it into a parser that always succeeds and returns a
`Maybe a`.

> optionMaybe :: Parser a -> Parser (Maybe a)
> optionMaybe p = Just <$> p <|> pure Nothing

A function that turns a parser into a pure function:

> parseOrDefault :: a -> Parser a -> Text -> a
> parseOrDefault def parser input =
>   case parseOnly parser input of
>     Left err -> def
>     Right x  -> x

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
>   return (pack chars)

Expressions for AnnotatedTexts
==============================

Getting AnnotatedText from a parser that produces Text:

> liftAnnotate :: Parser Text -> Parser AnnotatedText
> liftAnnotate combinator = annotFromText <$> combinator
