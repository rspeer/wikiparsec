> {-# LANGUAGE RankNTypes, NoMonomorphismRestriction, OverloadedStrings #-}

> module Text.MediaWiki.ParseTools where
> import qualified Data.Text as T
> import Data.Text (Text)
> import Text.Parsec
> import Text.Parsec.Text
> import Text.Parsec.Char
> import Text.Parsec.Pos
> import Control.Applicative ((<$>))

Parsing primitives for text
===========================

It puzzles me that this isn't already defined somewhere. Here's a parse rule
that matches a specific Text, the same way that `string` matches a String:

> matchText :: Text -> Parser Text
> matchText t = T.pack <$> string (T.unpack t)

The `<$>` operator, also known as "liftM", seems to be the preferred Haskell
way to apply a plain function to the output of a monadic computation, such as a
successful parse. Here, it "lifts" the `T.pack` function, of type `String ->
Text`, so that it turns the `Parser String` on the right side of `<$>` into
a `Parser Text`.

TODO: This seems inefficient. If I understood the internals of the `tokens`
combinator that `string` is built from, perhaps I could make a faster way of
comparing chunks of text.


Common parsing functions
========================

As part of many expressions, we need a quick way to discard what we matched
and use the empty text as its value:

> nop :: Parser Text
> nop = return ""

A lot of spans of Wikitext are mostly defined by what they're not. The
`textWithout` rule matches and returns a sequence of 1 or more characters that
are not in the given string.

> textWithout :: [Char] -> Parser Text
> textWithout chars = T.pack <$> many1 (noneOf chars)

The `symbol` rule matches a multi-character Text, but backtracks if it doesn't
match instead of getting stuck:

> symbol :: Text -> Parser Text
> symbol = try . matchText

This is similar to the `symbol` that's defined in Parsec's token-based
parse rules, but we're not importing those because they don't coexist with
significant whitespace.

A more complex version of this is when there are many different kinds of
strings that could appear in a given context, including different kinds of
strings concatenated together. For example, in one context, we might accept
plain text, links, and templates, but not line breaks.

To make this easier, we'll define `textChoices`, which takes a list of
expressions we're allowed to parse, tries all of them in that priority order,
and concatenates together their results. This expression *does not backtrack*,
so any sub-expression that starts to match should be prepared to finish the job
or should be wrapped in `try`.

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
> possiblyEmpty combinator = do
>   matched <- optionMaybe (try combinator)
>   case matched of
>     Just match -> return match
>     Nothing    -> nop

Sometimes a token starts some special environment that will consume everything
until an ending token. An example would be HTML comments, which consume
everything between `<!--` and `-->`.

We need to output something besides an error in the case where the ending token
never appears, though. What we choose to do is to consume everything up to the
end of the input, and return what we consumed.

> delimitedSpan :: Text -> Text -> Parser Text
> delimitedSpan open close = do
>   symbol open
>   chars <- manyTill anyChar (symbol close <|> (eof >> nop))
>   return (T.pack chars)

