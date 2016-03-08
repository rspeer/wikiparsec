Setup
=====

To parse the mess that is Wiktionary, we make use of Parsec, a well-regarded
parser-combinator library for Haskell.

Parsec is explicitly designed around the way Haskell works. I wouldn't
normally be using Haskell, but it does seem like the right tool for the job.

> module Text.MediaWiki.WikiText where
> import Text.Parsec hiding (parse, parseTest)
> import Text.Parsec.Char
> import Text.Parsec.Error (ParseError, errorPos)
> import Text.Parsec.Pos (sourceLine)
> import Debug.Trace (trace)

We're going to need to make use of Haskell's functional mapping type,
Data.Map, to represent the contents of templates.

> import Data.Map (Map)
> import qualified Data.Map as Map

Pull in some string-manipulating utilities that are defined elsewhere in
this package:

> import Text.Wiki.SplitUtils

Some common shorthand for defining parse rules:

> import Text.Wiki.ParseTools (Parser, (&>), symbol, textWithout, textChoices,
>                              textStrictChoices, concatMany, possiblyEmpty,
>                              delimitedSpan, nop)

And some more utilities from the MissingH package:

> import Data.String.Utils


Data types
==========

An internal link is represented as a record of a type we'll define here,
called `WikiLink`.

> data WikiLink = WikiLink {
>   namespace :: String,
>   page :: String,
>   section :: String
> } deriving (Show, Eq)

`makeLink` is a constant that can be used as a template for making WikiLinks.

> makeLink :: WikiLink
> makeLink = WikiLink {namespace="", page="", section=""}

Wiki links can be put together into linked text, which has a rendered
plain-text value and a list of links.

> data LinkedText = LinkedText [WikiLink] String deriving (Show, Eq)
>
> linkString :: WikiLink -> String -> LinkedText
> linkString link s = LinkedText [link] s
>
> unlinked :: String -> LinkedText
> unlinked s = LinkedText [] s
>
> addLinkedText :: LinkedText -> LinkedText -> LinkedText
> addLinkedText (LinkedText links1 s1) (LinkedText links2 s2) = LinkedText (links1 ++ links2) (s1 ++ s2)
>
> concatLinkedText :: [LinkedText] -> LinkedText
> concatLinkedText = foldl addLinkedText (unlinked "")
>
> discardLinks :: LinkedText -> String
> discardLinks (LinkedText links s) = s

An invocation of a template is represented as a Map from parameter names to
values.  Both the names and the values are strings.

> type TemplateData = Map String String


Spans of text
=============

Some formatting allows whitespace as long as it stays on the same line --
for example, the whitespace around headings and after list bullets.

> sameLineSpaces :: Parser ()
> sameLineSpaces = skipMany (oneOf " \t")

Our most reusable expression for miscellaneous text, `basicText`, matches
characters that aren't involved in any interesting Wiki syntax.

But what about the *uninteresting* Wiki syntax? Any span of Wikitext can
have double or triple apostrophes in it to indicate bold and italic text.
Single apostrophes are, of course, just apostrophes.

We could modify every parse rule that handles basic text to also have a case
for bold and italic spans and an exception for individual apostrophes, but
instead, we could take advantage of the fact that these spans are at the lowest
level of syntax and we want to ignore them anyway.

We'll just post-process the parse result to remove the sequences of
apostrophes, by chaining it through the `discardSpans` function. (See
`ParseTools.lhs` for the definition of the `&>` operator.)

> basicText :: Parser String
> basicText = textWithout "[]{}|:=\n" &> discardSpans
>
> discardSpans :: String -> String
> discardSpans = (replace "''" "") . (replace "'''" "")

There's a quirk in Wiki syntax: things that would cause syntax errors just get
output as themselves. So sometimes, some of the characters excluded by
`basicText` are going to appear as plain text, even in contexts where they would
have a meaning -- such as a single closing bracket when two closing brackets
would end a link.

It would be excessive to actually try to simulate MediaWiki's error handling,
but we can write this expression that allows "loose brackets" to be matched
as text:

> looseBracket :: Parser String
> looseBracket = (
>   do
>     bracket <- oneOf "[]{}"
>     notFollowedBy (char bracket)
>     return [bracket]
>   ) <?> "unmatched bracket"

Wikitext in general is made of HTML, links, templates, and miscellaneous text.
We'll parse templates for their meaning below, but in situations where we don't
care about templates, we simply discard their contents using the
`ignoredTemplate` rule.

> wikiTextLine :: Parser String
> wikiTextLine = textStrictChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, looseBracket, textLine] <?> "line of wikitext"
>
> wikiNonHeadingLine :: Parser String
> wikiNonHeadingLine = textStrictChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, looseBracket, nonHeadingLine] <?> "non-heading line of wikitext"
>
> wikiText :: Parser String
> wikiText = textChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, looseBracket, nonHeadingLine, nonHeadingNewline] <?> "wikitext"
> cleanWikiText = textStrictChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, nonHeadingLine, nonHeadingNewline] <?> "well-formed wikitext"
> textLine = textWithout "[]{}\n" &> discardSpans <?> "line of plain text"
> nonHeadingLine = notFollowedBy (char '=') >> textLine <?> "non-heading line"
> nonHeadingNewline = notFollowedBy (char '=') >> newLine <?> "non-heading newline"
>
> newLine :: Parser String
> newLine = string "\n"
>
> eol :: Parser ()
> eol = (newLine >> return ()) <|> eof


Wiki syntax items
=================

Links
-----

External links appear in single brackets. They contain a URL, a space, and
the text that labels the link, such as:

    In:  [http://www.americanscientist.org/authors/detail/david-van-tassel David Van Tassel]
    Out: "David Van Tassel"

External links can have no text, in which case they just get an arbitrary
number as their text, which we'll disregard. There's also a type of external
link that is just a bare URL in the text. Its effect on the text is exactly
the same as if it weren't a link, so we can disregard that case.

The following rules extract the text of an external link, as both `between`
and `do` return what their last argument matches.

> externalLinkText :: Parser String
> externalLinkText = try (between (string "[") (string "]") externalLinkContents)
> externalLinkContents = do
>   schema
>   urlPath
>   spaces
>   cleanWikiText
> schema = choice (map symbol ["http://", "https://", "ftp://", "news://", "irc://", "mailto:", "//"])
> urlPath = textWithout "[]{}| "

FIXME: stop describing LinkState.

Internal links have many possible components. In general, they take the form:

    [[namespace:page#section|label]]

The only part that has to be present is the page name. If the label is not
given, then the label is the same as the page.

When parsing internal links, we return just their label. However, other
details of the link are added to the LinkState.

      In:    [[word]]
      Out:   "word"
      State: [makeLink {page="word"}]

      In:    [[word|this word]]
      Out:   "this word"
      State: [makeLink {page="word"}]

      In:    [[word#English]]
      Out:   "word"
      State: [makeLink {page="word", section="English"}]

      In:    [[w:en:Word]]
      Out:   "word"
      State: [makeLink {namespace="w:en", page="word"}]

      In:    [[Category:English nouns]]
      Out:   ""
      State: [makeLink {namespace="Category", page="English nouns"}]


> internalLink :: Parser LinkedText
> internalLink = between (symbol "[[") (symbol "]]") internalLinkContents
> internalLinkContents = do
>   target <- linkTarget
>   maybeText <- optionMaybe alternateText
>   let link = (parseLink target) in do
>     case (namespace link) of
>       -- Certain namespaces have special links that make their text disappear
>       "Image"    -> return (linkString link "")
>       "Category" -> return (linkString link "")
>       "File"     -> return (linkString link "")
>       -- If the text didn't disappear, find the text that labels the link
>       _          -> case maybeText of
>         Just text  -> return (linkString link text)
>         -- With no alternate text, the text is the name of the target page
>         Nothing    -> return (linkString link (page link))
>
> internalLinkText :: Parser String
> internalLinkText = internalLink &> discardLinks
>
> linkTarget :: Parser String
> linkTarget = textWithout "[]{}|\n"
>
> alternateText = char '|' >> wikiText
>
> parseLink :: String -> WikiLink
> parseLink target =
>   WikiLink {namespace=namespace, page=page, section=section}
>   where
>     (namespace, local) = splitLast ':' target
>     (page, section) = splitFirst '#' local
>
> linkedWikiText :: Parser LinkedText
> linkedWikiText = (many linkedWikiTextPiece) &> concatLinkedText
> linkedWikiTextPiece = internalLink <|> unlinkedWikiText
> unlinkedWikiText = textStrictChoices [externalLinkText, ignoredTemplate, nonHeadingLine, nonHeadingNewline] &> unlinked

Headings
--------

When parsing an entire Wiki article, you'll need to identify where the
headings are. This is especially true on Wiktionary, where the
domain-specific parsing rules will change based on the heading.

The `heading` parser looks for a heading of a particular level (for
example, a level-2 heading is one delimited by `==`), and returns its
title.

> heading :: Int -> Parser String
> heading level =
>   let delimiter = (replicate level '=') in do
>     try (string delimiter >> notFollowedBy (char '='))
>     optional sameLineSpaces
>     text <- manyTill headingChar (symbol delimiter)
>     optional sameLineSpaces
>     newLine
>     return (rstrip text)
>
> headingChar = noneOf "\n"

Some parse rules expect to find a heading that matches a particular rule:

> specificHeading level titleRule =
>   let delimiter = (replicate level '=') in do
>     try (string delimiter >> notFollowedBy (char '='))
>     optional sameLineSpaces
>     title <- titleRule
>     optional sameLineSpaces
>     symbol delimiter
>     optional sameLineSpaces
>     newLine
>     return title

Lists
-----

Here's a hierarchical data type for describing the contents of lists, which
semantically can contain other lists.

> data ListItem = Item String
>               | ListHeading String
>               | BulletList [ListItem]
>               | OrderedList [ListItem]
>               | IndentedList [ListItem]
>               deriving (Show, Eq)

Sometimes we just want the text that the list contains. `extractText`
returns the text of the list items (whatever kind of items they are) separated
by line breaks.

> extractText :: ListItem -> String
> extractText (Item s) = s
> extractText (ListHeading s) = s
> extractText (BulletList items) = extractTextFromList items
> extractText (OrderedList items) = extractTextFromList items
> extractText (IndentedList items) = extractTextFromList items
>
> extractTextFromList :: [ListItem] -> String
> extractTextFromList items = unlines (map extractText items)

> listItems :: String -> Parser [ListItem]
> listItems marker = do
>   lookAhead (string marker)
>   many1 (listItem marker)
>
> listItem :: String -> Parser ListItem
> listItem marker = subList marker <|> singleListItem marker
>
> subList :: String -> Parser ListItem
> subList marker =   try (bulletList (marker ++ "*"))
>                <|> try (orderedList (marker ++ "#"))
>                <|> try (indentedList (marker ++ ":"))
>                <|> try (listHeading (marker ++ ";"))
>
> anyList :: Parser ListItem
> anyList = subList ""
>
> anyListText :: Parser String
> anyListText = anyList &> extractText <?> "list"
>
> listHeading :: String -> Parser ListItem
> listHeading marker = listItemContent marker &> ListHeading
>
> singleListItem :: String -> Parser ListItem
> singleListItem marker = listItemContent marker &> Item
>
> listItemContent :: String -> Parser String
> listItemContent marker = do
>   symbol marker
>   optional sameLineSpaces
>   line <- wikiTextLine
>   eol
>   return line
>
> bulletList marker   = listItems marker &> BulletList
> orderedList marker  = listItems marker &> OrderedList
> indentedList marker = listItems marker &> IndentedList
>
> isPlainItem :: ListItem -> Bool
> isPlainItem (Item s) = True
> isPlainItem _ = False


Templates
---------

A simple template looks like this:

    {{archaic}}

More complex templates take arguments, such as this translation into French:

    {{t+|fr|exemple|m}}

And very complex templates can have both positional and named arguments:

    {{t|ja|例え|tr=[[たとえ]], tatoe}}

Some templates are more detailed versions of internal links. Some are metadata
that we can simply ignore. The ultimate semantics of a template can depend both
on its contents and the section in which it appears, so these semantics need to
be defined in the parsing rules for a specific wiki such as the English
Wiktionary.

Here, we define the basic syntax of templates, and return their contents in a
standardized form as a mapping from argument names to values.

> template :: Parser TemplateData
> template = symbol "{{" >> (templateArgs 0)
>
> ignoredTemplate :: Parser String
> ignoredTemplate = template >> nop

> templateArgs :: Int -> Parser TemplateData
> templateArgs offset = do
>   nameMaybe <- optionMaybe (try templateArgName)
>   case nameMaybe of
>     Just name -> namedArg name offset
>     Nothing -> positionalArg offset
>
> templateArgName :: Parser String
> templateArgName = do
>   name <- basicText
>   string "="
>   return name
>
> namedArg :: String -> Int -> Parser TemplateData
> namedArg name offset = do
>   value <- possiblyEmpty wikiTextArg
>   rest <- templateRest offset
>   return (Map.insert name value rest)
>
> positionalArg :: Int -> Parser TemplateData
> positionalArg offset = do
>   value <- possiblyEmpty wikiTextArg
>   rest <- templateRest (offset + 1)
>   return (Map.insert (show offset) value rest)
>
> templateRest :: Int -> Parser TemplateData
> templateRest offset = endOfTemplate <|> (string "|" >> templateArgs offset)
>
> endOfTemplate = symbol "}}" >> return Map.empty
>
> wikiTextArg = textChoices [internalLinkText, externalLinkText, ignoredTemplate, looseBracket, textArg]
> textArg = textWithout "[]{}|" &> discardSpans

We can simplify some of this parsing in the case where we are looking for a
*particular* template. We start by expecting two left braces and the name
of the template, then parse the rest of the template as usual.

We set the template name as arg 0, as it would be if we were using the more
general rule for parsing template expressions.

> specificTemplate :: String -> Parser TemplateData
> specificTemplate name = do
>   symbol ("{{" ++ name)
>   parsed <- templateRest 1
>   return (Map.insert "0" name parsed)


Tables
------

Tables have complex formatting, and thus far we're just going to be skipping
them.

> wikiTable :: Parser String
> wikiTable = (wikiTableComplete <|> (try wikiTableDetritus >> nop))
>
> wikiTableComplete :: Parser String
> wikiTableComplete = delimitedSpan "{|" "|}" >> nop

MediaWiki templates can be used in horrifying ways, and one way that they're
sometimes used (particularly on Wikipedia) is to start a table that is then
ended outside the template.

We don't know which templates leave tables hanging open, but when we skip over
such a template, we'll see a bunch of lines starting with |. Lines intended as
text don't normally start with vertical bars. So we can clean up incomplete
tables by skipping over all such lines.

> wikiTableDetritus :: Parser ()
> wikiTableDetritus = do
>   newLine
>   string "|"
>   many (noneOf "\n")
>   try wikiTableDetritus <|> eol


Parsing sections at a time
--------------------------

These functions are designed to take in entire sections of wikitext,
or an entire page, and return the plain text that they contain.

> sectionText :: Int -> Parser String
> sectionText level = do
>   theHeading <- try (optional newLine >> heading level) <?> ("level-" ++ (show level) ++ " heading")
>   theContent <- sectionContent level <?> "section content"
>   newLine <|> (eof >> nop) <?> "end of line"
>   return (squishBlankLines (unlines [theHeading, "", theContent]))
>
> sectionContent level = textStrictChoices [sectionText (level + 1), sectionContent' level]
> sectionContent' level = textChoices [anyListText, wikiNonHeadingLine, newLine] <?> ("level-" ++ (show level) ++ " section content")

A page usually acts like the content of a level-1 section, without a heading
(because the heading is actually the page title). However, a Wiki page can
also contain level-1 headings, though it's discouraged. If we encounter a
level-1 heading, then the level-1 section we're parsing will end, but at
that point we can begin parsing a new level-1 section including the heading.

> pageText :: Parser String
> pageText = do
>   content <- textChoices [sectionText 1, sectionContent 1]
>   eof
>   return (squishBlankLines content)
>
> squishBlankLines :: String -> String
> squishBlankLines s = unlines (filter isNonEmptyString (lines s))
>
> isNonEmptyString :: String -> Bool
> isNonEmptyString s = (length s) > 0


Entry points
------------

Parsec defines useful helpers such as parseTest, but they require the parser
to have no modifiable state. We care a lot about the modifiable state, so
we'll write our own version.

> parseTest parser input =
>   let sourceName = "(test)" in
>     case parse parser sourceName input of
>       Left err -> do
>         putStr "parse error at "
>         print err
>       Right x -> do
>         print x
>
> parse parser = runParser parser ()

Here's a function to be run at the IO level, which takes in a string of Wikitext,
outputs its plain text, and returns nothing.

> outputPlainText :: String -> IO ()
> outputPlainText input =
>   let sourceName = "(input)" in
>     case parse pageText sourceName input of
>       Left err -> showError input err
>       Right x -> putStrLn x

Showing informative errors:

> showError :: String -> ParseError -> IO ()
> showError str err =
>   let strLines  = lines str
>       errorLine = sourceLine (errorPos err)
>       errorCol  = sourceColumn (errorPos err)
>   in do
>     putStrLn "********"
>     putStr "parse error at "
>     print err
>     putStrLn (strLines !! (errorLine - 1))
>     putStr (replicate (errorCol - 1) ' ')
>     putStrLn "^"
>     putStrLn "\n"
>     putStrLn str
>     putStrLn "********"
