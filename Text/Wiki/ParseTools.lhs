> {-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

> module Text.Wiki.ParseTools where
> import Control.Monad.Identity (liftM)
> import Text.Parsec hiding (parse, parseTest)
> import Text.Parsec.Char

Parser-making expressions
-------------------------

As part of many expressions, we need a quick way to discard what we matched
and use the empty string as its value:

> nop = return ""

Ugly type expression:

> type GeneralizedParser = forall state. Parsec String state

The awkward thing about LL parsing is that you can consume part of a string,
fail to match the rest of it, and be unable to backtrack. When we match a
multi-character string, we usually want it to be an all-or-nothing thing. At
the cost of a bit of efficiency, we'll use the `symbol` expression for
multi-character strings, which wraps the `string` parse rule in `try` so it can
backtrack.

> symbol = try . string

A lot of spans of Wikitext are mostly defined by what they're not. The
`textWithout` rule matches and returns a sequence of 1 or more characters that
are not in the given string.

> textWithout :: String -> GeneralizedParser String
> textWithout chars = many1 (noneOf chars)

This is similar to the `symbol` that's defined in Parsec's token-based
parse rules, but we're not importing those because they don't coexist with
significant whitespace.

A more complex version of this is when there are many different kinds of
strings that could appear in a given context, including different kinds of
strings concatenated together. For example, in one context, we might accept
plain text, links, and templates, but not line breaks.

To make this easier, we'll define `textChoices`, which takes a list of
expressions we're allowed to parse, tries all of them in that priority order
(backtracking whenever one fails), and concatenates together their results.
`textStrictChoices` is similar, but does not backtrack.

> textChoices :: [GeneralizedParser String] -> GeneralizedParser String
> textChoices options = concatMany (choice (map try options))
>
> textStrictChoices :: [GeneralizedParser String] -> GeneralizedParser String
> textStrictChoices options = concatMany (choice options)
>
> concatMany :: GeneralizedParser String -> GeneralizedParser String
> concatMany combinator = do
>   parts <- many1 combinator
>   return (concat parts)

Most of the expressions we write will match at least one character, allowing
us to repeat them without allowing repeated matches of the empty string.
However, there are cases where the empty string is a valid value for a
sub-expression. In those cases, we wrap the sub-expression in `possiblyEmpty`.

> possiblyEmpty :: GeneralizedParser String -> GeneralizedParser String
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

> delimitedSpan :: String -> String -> GeneralizedParser String
> delimitedSpan open close = do
>   symbol open
>   manyTill anyChar (symbol close <|> (eof >> nop))


The "and-then" operator
-----------------------

I'm going to define a new operator that's going to be pretty useful in a lot of
these expressions. Often I have a function that's in some monad, like `Parser
String` for a suitably-defined type expression `Parser`, and I want to apply a
transformation to its output, like `String -> String`.

The `liftM` function almost does this: it converts `String -> String`
to `Parser String -> Parser String`, for example. But it's just a function,
and you apply functions on the left... so the last thing you do has to be the
first thing you write. This is confusing because the rest of the parser
expression is usually written in sequential order, especially when it's using
`do` syntax.

So this operator, the "and-then" operator, lets me write the thing that needs
to happen to the output at the end. I could just define it as `(flip liftM)`, but
that would be pointless. (Functional programming puns! Hooray!)

> (&>) :: Monad m => m a -> (a -> b) -> m b
> (&>) result f = liftM f result

