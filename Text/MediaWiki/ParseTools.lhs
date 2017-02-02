`Text.MediaWiki.ParseTools`: useful parser expressions for Attoparsec
=====================================================================

We're going to be parsing WikiText using Attoparsec, a parser combinator
library that's a faster, leaner, less flexible version of Parsec.

Attoparsec expressions have the type `Parser a`, where `a` is the type of the
value they result in. Unlike in Parsec, we don't have to fill in type
parameters to say that the input we're parsing is Text; we do that by importing
our functions from `Data.Attoparsec.Text` instead of, say,
`Data.Attoparsec.ByteString`.

> {-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}

> module Text.MediaWiki.ParseTools where
> import WikiPrelude hiding (takeWhile)
> import Text.MediaWiki.AnnotatedText
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator


Sensible names for things
-------------------------

Let's rename the functions that add a character to the start or end of a
sequence, for people who don't keep a copy of SICP under their pillow:

> prependChar :: Char -> Text -> Text
> prependChar = cons
>
> unPrependChar :: Text -> Maybe (Char, Text)
> unPrependChar = uncons
>
> appendChar :: Text -> Char -> Text
> appendChar = snoc

As part of many expressions, we need a quick way to discard what we matched and
return an empty value, such as a Text parser that returns the empty text. We
get an appropriate empty value from the fact that our value's type is a Monoid.

> nop :: (Monoid a) => Parser a
> nop = return ø


Common parsing functions
------------------------

Spans of Wikitext are often distinguished by what characters they contain, or
what characters they don't contain. `textWith` parses a sequence of characters
that are in the given list, but fails if 0 characters match.

`takeWhile1` is the Attoparsec combinator that parses characters as long as a
certain `(Char -> Bool)` predicate is satisfied, and fails if it parses 0
characters.

> textWith :: [Char] -> Parser Text
> textWith chars = takeWhile1 (inClass chars)

`skipChars` is similar to `textWith`, but indicates that we don't care about
what the characters are, and don't want to return a value.

> skipChars :: [Char] -> Parser ()
> skipChars chars = skipWhile (inClass chars)

`textWithout` is, naturally, the opposite of `textWith`, matching 1 or more
characters that are not in the given list.

> textWithout :: [Char] -> Parser Text
> textWithout chars = takeTill1 (inClass chars)

Surprisingly, Attoparsec defines `takeWhile`, `takeWhile1`, and `takeTill`, but
not `takeTill1`, so we have to define it -- we check that one character
satisfies `not . pred`, then match the rest with `takeTill`.

> takeTill1 :: (Char -> Bool) -> Parser Text
> takeTill1 pred = do
>   c    <- satisfy (not . pred)
>   rest <- takeTill pred
>   return (prependChar c rest)

Sometimes there are many different kinds of strings that could appear in a
given context, including different kinds of strings concatenated together. For
example, in one context, we might accept plain text, links, and templates, but
not line breaks.

To make this easier, we'll define `textChoices`, which takes a list of
expressions we're allowed to parse, tries all of them in that priority order,
and concatenates together their results.

The built-in combinators we're using are `choice`, which tries several parser
combinators in order and uses the first one that succeeds, and `many1`, which
applies a combinator one or more times, and builds up a list of the results.
For the `optionalTextChoices` version, we instead use `many'`, which matches 0
or more times. (Don't ask about the apostrophe.)

> textChoices :: (Monoid a) => [Parser a] -> Parser a
> textChoices options = concat <$> many1 (choice options)
>
> optionalTextChoices :: (Monoid a) => [Parser a] -> Parser a
> optionalTextChoices options = concat <$> many' (choice options)

Most of the expressions we write will match at least one character, allowing
us to repeat them without allowing repeated matches of the empty string.
However, there are cases where the empty string is a valid value for a
sub-expression. In those cases, we wrap the sub-expression in `possiblyEmpty`,
and return the empty value of the monoid if it fails.

> possiblyEmpty :: (Monoid a) => Parser a -> Parser a
> possiblyEmpty combinator = option ø combinator

`notFollowedByChar` looks ahead one character to make sure it's not the
particular character `c`.

> notFollowedByChar :: Char -> Parser ()
> notFollowedByChar c = do
>   maybeChar <- peekChar
>   case maybeChar of
>     Nothing -> return ()
>     Just c' -> if (c == c') then empty else return ()

`optionMaybe` takes a parser for `a` that might fail, and turns it into a
parser that always succeeds and returns a `Maybe a`.

> optionMaybe :: Parser a -> Parser (Maybe a)
> optionMaybe p = (Just <$> p) <|> return Nothing

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
------------------------------

Getting AnnotatedText from a parser that produces Text:

> liftAnnotate :: Parser Text -> Parser AnnotatedText
> liftAnnotate combinator = annotFromText <$> combinator

Unwrapping parse results
------------------------

Building up parsers is great, but at some point we'll get a value that we just
want to use as a value, not wrapped in a monad to be used in a larger parser.

Attoparsec provides `parseOnly` for this, which wraps the value in an `Option`
so it can return an error if the text didn't parse. It's easier in some cases
to just return the value, with a default value if it didn't parse, so we define
that as `parseOrDefault`.

The three arguments of `parseOrDefault` are the default value, the parser
combinator, and the text to parse with it.

> parseOrDefault :: a -> Parser a -> Text -> a
> parseOrDefault def parser input =
>   case parseOnly parser input of
>     Left err -> def
>     Right x  -> x

