`Text.MediaWiki.Sections`: separating the sections of a Wiki page
=================================================================

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax #-}

In an ideal world, this parsing step wouldn't be necessary. Wikitext
headings would just be markup that we could parse along with the rest
of the page.

But here we are, reimplementing a monstrous PHP parser that's defined
by its behavior in Haskell. This is *clearly* not an ideal world.

We parse sections in this separate step for two reasons:

- Wikitext constructs can have very different meanings depending on what
  section they appear in. On Wiktionary, for example, a numbered list
  could mean lots of things, but if it's in a level-3 or level-4 section
  named with a part of speech, it's the list of definitions of the word.

- If something goes wrong in parsing Wikitext, dividing the page into
  sections can put the brakes on it. Instead of failing to parse the rest
  of the page, we recover at the start of the next section.

We'll be parsing sections using a Parsec parser that consumes a line at a time.
(Not Attoparsec, which we'll be using for the more detailed parsing of Wikitext
-- in this case we need Parsec's flexibility about what data structure its
input is made of).

Parsec builds parsing rules out of Haskell functions (combinators). One of
these combinators is called `many`. Unfortunately, that's also the name of a
function in the Classy Prelude. We need Parsec's version of `many` so we hide
the one we got via WikiPrelude.

> module Text.MediaWiki.Sections where
> import WikiPrelude hiding (many)
> import Text.Parsec.Pos
> import Text.Parsec.Prim
> import Text.Parsec.Combinator
> import Text.Parsec.Error


Data structures
---------------

First we break the text into lines. Then we group these lines into
individual sections with their headings. Finally, we step through the
sections, converting them into contextualized WikiSection objects that know
about their entire stack of headings.

Here we define the data structures representing the outputs of these various
steps.

A `TextLine` is either a `Heading` or `Plain`. If it's a `Heading`, it contains
a number indicating its level, along with its title text. The heading
`== Example ==` would become `(Heading 2 "Example")`.

"`Plain Text`" makes a nice noun phrase, but remember that it's just a similar
construction to `Heading Int Text`. It just means that there's a version of
`TextLine` called `Plain`, whose single value has type `Text`.

> data TextLine = Heading Int Text | Plain Text deriving (Eq, Show)
>
> isHeading :: TextLine -> Bool
> isHeading (Heading _ _) = True
> isHeading _             = False

`getText` gets the text out of a TextLine.

> getText :: TextLine -> Text
> getText (Plain text) = text
> getText (Heading level text) = text

When we parse a section full of lines, we represent it as a `SingleSection`
record, containing values for its level, the title of its heading, and the text
of its content.

> data SingleSection = SingleSection {
>   ssLevel :: Int,
>   ssHeading :: Text,
>   ssContent :: Text
> } deriving (Eq, Show)

A WikiSection is a SingleSection in context. It contains not just its own
title, but the titles of all higher-level sections that contain it.

It doesn't need to keep track of its level as a number, because that's just the
length of the `headings` list.

> data WikiSection = WikiSection {
>   headings :: [Text],
>   content :: Text
> } deriving (Eq, Show)


Reading lines
-------------

This is a kind of lexer for the section parser. We sort the lines of the page
into two types: headings and non-headings.

> readLines :: Text -> [TextLine]
> readLines text = map parseTextLine (lines text)

On some Wikimedia projects, the sections are separated by horizontal rules (I guess
the section formatting wasn't enough). This is semantically meaningless, so replace
the rules (written as four or more dashes) with blank lines.

We parse each line by trying to parse it as a heading, and keeping track of its
*level*, the number of equals signs surrounding it. If we get level 0, it wasn't a
heading at all, it's plain text. Either way, return the `TextLine` structure
for what we found on the line.

> parseTextLine :: Text -> TextLine
> parseTextLine text =
>   if isPrefixOf "----" text
>     then (Plain "")   -- remove horizontal rules
>     else
>       let (innerText, level) = headingWithLevel (stripSpaces text)
>       in  (if level == 0 then (Plain text) else (Heading level (stripSpaces innerText)))

`headingWithLevel` recursively removes equals signs from the left and right
side of the line, returning the remaining text and the number of removed pairs
of equals signs.

> headingWithLevel :: Text -> (Text, Int)
> headingWithLevel text =
>   case trimEquals text of
>     Just trimmed ->
>       let (finalText, innerLevel) = headingWithLevel trimmed in
>         (finalText, innerLevel + 1)
>     Nothing -> (text, 0)

`trimEquals` takes in a text, and if it can trim an equals sign from each end,
returns `Just` the trimmed result. Otherwise, it returns `Nothing`.

> trimEquals :: Text -> Maybe Text
> trimEquals text =
>   case stripPrefix "=" text of
>     Nothing -> Nothing
>     Just x  -> stripSuffix "=" x


A line-by-line parser
---------------------

`Parsec` is a parsing monad with a lot of parameters:

- The first parameter is the data type that it takes as input.
- The second parameter is the type of the internal state it keeps while parsing.
- The third parameter is the type that the value of an expression should have
  when it parses successfully.

The third parameter typically varies from one parsing expression to another, while
the first two stay fixed, so a typical thing to do is to define a more specific
type that fills in the first two parameters.

In this case, a LineParser takes in a list of TextLines, and keeps no state.

> type LineParser = Parsec [TextLine] ()

Our parsing expressions will all be wrapped in this monad, because the monad
keeps track of how to backtrack and try something else when a particular
expression fails to parse. So if a function uses the `LineParser` monad
and returns a `Text` value if the parse succeeds, its return type is
`LineParser Text`.

Usually, you'd build up a parser from parsing primitives that Parsec provides,
such as matching literal strings or character classes. But none of those
primitives are defined on [TextLine], a type we just made up. So we need to
define our own primitive, which is a bit cumbersome.

Defining a primitive requires defining three functions, two of which are only
there for debugging:

- `showLine`: how this value should be shown in debugging output
- `nextPos`: how to keep track of the line and column number of the input
- `test` (in this case `testLine`): actually decides whether something parses
  or not. Returns `Just value` if it parses, and `Nothing` if not.

Our primitive, `matchLine`, takes in a predicate of type (TextLine -> Bool),
and successfully parses lines where that predicate returns `true`, returning
the line as the value.

> matchLine :: (TextLine -> Bool) -> LineParser TextLine
> matchLine pred =
>   let showLine = show
>       nextPos pos x xs = updatePosLine pos x
>       testLine line = if pred line then Just line else Nothing
>   in  tokenPrim showLine nextPos testLine

Because we have to track line and column numbers, we just borrow a Parsec
function named `incSourceLine` to increment the line number each time we parse
a line. The column number is always 1.

> updatePosLine :: SourcePos -> TextLine -> SourcePos
> updatePosLine pos _ = incSourceLine pos 1

Now we can use our primitive to define two parsing expressions. One matches
plain text lines (returning their text), and the other matches headings
(returning the `Heading` structure).

> pPlainLine :: LineParser Text
> pPlainLine = getText <$> matchLine (not . isHeading)
>
> pHeadingLine :: LineParser TextLine
> pHeadingLine = matchLine isHeading


Parsing sections
----------------

Okay. We've defined a parsing monad, `LineParser`, and some expressions that
use it. Now we get to actually use Parsec to make things easier. This is the
first parser being introduced, and the simplest, so I'll explain it in detail.

`LineParser` is a monad, so we can use `do`-notation to describe a sequence of
things to do with it. We get to write the sequence as if every step succeeds,
and we get to use the monad assignment operator `<-` to assign names to the
*unwrapped* values that result from the successful parses.

In short, we get to just describe the happy path. Any other path causes the
result of the whole `do` expression to be a failed parse, which is exactly
what we want.

`pSection` parses an entire section, returning a `SingleSection` value. It
first parses the heading, then uses the `many` combinator to parse plain lines
until it fails (because it reached another heading or the end of the page).

`many pPlainLine` returns a list of the lines that were parsed, which we
can glue back together with the standard function `unlines`.

In the end, we `return` the `SingleSection` value. Unlike in other programming
languages, `return` doesn't have anything to do with the flow of the program.
It just means "wrap this value in the monad".

> pSection :: LineParser SingleSection
> pSection = do
>   Heading level name <- pHeadingLine
>   textLines <- many pPlainLine
>   return (SingleSection { ssLevel = level, ssHeading = name, ssContent = unlines textLines })


Converting sections
-------------------

Here's how we convert a list of SingleSections into a list of contextualized
WikiSections.

`processSectionHeadings` is a recursive function that uses its first argument,
of type `[Text]`, to track the current stack of headings.

(Note: we're keeping the stack in order from left to right, and altering the
end of it. Probably the more functional-linked-listy thing to do would be to
store the stack backwards, so we could alter the front of it with the operations
that are most efficient on linked lists. Oh well.)

> convertSections :: [SingleSection] -> [WikiSection]
> convertSections = processSectionHeadings []

Define the usual base case: if there are no sections left in the input,
there are no sections left to return.

> processSectionHeadings :: [Text] -> [SingleSection] -> [WikiSection]
> processSectionHeadings headingStack [] = []

We split off the first section, `sec`, from the remaining input, and use
`applyHeadings` to turn it into a WikiSection. We then recurse, with
our `headingsStack` now being `sec`'s set of headings.

> processSectionHeadings headingStack (sec:rest) =
>   let sec' = (applyHeadings headingStack sec)
>       heds = (headings sec')
>   in  (sec':(processSectionHeadings heds rest))

`applyHeadings` takes in a previous stack of headings and a new section.
When this section has a heading level of *n*, the new heading stack will
be the existing *n - 1* headings above it, followed by the new heading.

> applyHeadings :: [Text] -> SingleSection -> WikiSection
> applyHeadings headingStack sec =
>   let heds = (take ((ssLevel sec) - 1) headingStack) ++ [ssHeading sec]
>   in  WikiSection { headings = heds, content = ssContent sec }


Parsing the whole page
----------------------

It's convenient for us if all text is in a section. The text that precedes any
section headings is effectively in a level-1 section called "top". Let's just
add the heading for it before we scan its lines.

> preparePage :: Text -> [TextLine]
> preparePage text = readLines ("=top=\n" ⊕ text)
>
> pPage :: LineParser [WikiSection]
> pPage = convertSections <$> many pSection
>
> parsePageIntoSections :: Text -> [WikiSection]
> parsePageIntoSections text =
>   case (parse pPage "" (preparePage text)) of
>     Left err       -> []
>     Right sections -> sections

