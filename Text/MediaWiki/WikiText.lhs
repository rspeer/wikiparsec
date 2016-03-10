> {-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

Setup
=====

To parse the mess that is Wiktionary, we make use of Parsec, a well-regarded
parser-combinator library for Haskell.

Parsec is explicitly designed around the way Haskell works. I wouldn't
normally be using Haskell, but it does seem like the right tool for the job.

> module Text.MediaWiki.WikiText where
> import qualified Data.Text as T
> import Data.Text (Text)
> import Text.Parsec
> import Text.Parsec.Error (ParseError, errorPos)
> import Text.Parsec.Pos (sourceLine)
> import Text.Parsec.Char
> import Text.Parsec.Text
> import Debug.Trace (trace)
> import Control.Applicative ((<$>))

We're going to need to make use of Haskell's functional mapping type,
Data.Map, to represent the contents of templates.

> import Data.Map (Map)
> import qualified Data.Map as Map

Pull in some string-manipulating utilities that are defined elsewhere in
this package:

> import Text.MediaWiki.SplitUtils (tSplitFirst, tSplitLast)

Some common shorthand for defining parse rules:

> import Text.MediaWiki.ParseTools
>   (matchText, symbol, nop, textWithout, textChoices, concatMany,
>    possiblyEmpty, delimitedSpan)


Data types
==========

An internal link is represented as a record of a type we'll define here,
called `WikiLink`.

> data WikiLink = WikiLink {
>   namespace :: Text,
>   page :: Text,
>   section :: Text
> } deriving (Show, Eq)

`makeLink` is a constant that can be used as a template for making WikiLinks.

> makeLink :: WikiLink
> makeLink = WikiLink {namespace="", page="", section=""}

Wiki links can be put together into linked text, which has a rendered
plain-text value and a list of links.

> data LinkedText = LinkedText [WikiLink] Text deriving (Show, Eq)
>
> linkText :: WikiLink -> Text -> LinkedText
> linkText link s = LinkedText [link] s
>
> unlinked :: Text -> LinkedText
> unlinked s = LinkedText [] s
>
> emptyLinkedText :: LinkedText
> emptyLinkedText = unlinked ""
>
> addLinkedText :: LinkedText -> LinkedText -> LinkedText
> addLinkedText (LinkedText links1 s1) (LinkedText links2 s2) = LinkedText (links1 ++ links2) (T.append s1 s2)
>
> addLinkedTextSep :: Text -> LinkedText -> LinkedText -> LinkedText
> addLinkedTextSep sep (LinkedText links1 s1) (LinkedText links2 s2) = LinkedText (links1 ++ links2) (T.append (T.append s1 sep) s2)
>
> concatLinkedText :: [LinkedText] -> LinkedText
> concatLinkedText = foldl addLinkedText emptyLinkedText
>
> joinLinkedText :: Text -> [LinkedText] -> LinkedText
> joinLinkedText sep [] = emptyLinkedText
> joinLinkedText sep ts = foldl1 (addLinkedTextSep sep) ts
>
> discardLinks :: LinkedText -> Text
> discardLinks (LinkedText links s) = s

An invocation of a template is represented as a Map from parameter names to
values.  Both the names and the values are Text.

> type TemplateData = Map Text Text


Spans of text
=============

Some formatting allows whitespace as long as it stays on the same line --
for example, the whitespace around headings and after list bullets.

> sameLineSpaces :: Parser ()
> sameLineSpaces = skipMany (oneOf " \t")

Here we're going to define some parsers that scan through characters, within a
line, that aren't involved in any interesting Wiki syntax.

We don't worry about apostrophes here, which are perhaps the least interesting
level of Wiki syntax.  Any span of Wikitext can have double or triple
apostrophes in it to indicate bold and italic text.  Single apostrophes are, of
course, just apostrophes.

We could modify every parse rule that handles basic text to also have a case
for bold and italic spans and an exception for individual apostrophes, but
instead, we could take advantage of the fact that these spans are at the lowest
level of syntax and we want to ignore them anyway.

We'll just post-process the parse result to remove the sequences of
apostrophes, by chaining it through the `discardSpans` function. (See
`ParseTools.lhs` for the definition of the `<$>` operator.)

> discardSpans :: Text -> Text
> discardSpans = (T.replace "''" "") . (T.replace "'''" "")

What we count as plain text has to depend on what environment we're in, such as
whether we're currently parsing a link or a template.

> plainText :: Parser Text
> plainText           = discardSpans <$> textWithout "[]{}\n"
> plainTextInTemplate = discardSpans <$> textWithout "[]{}|\n"
> plainTextInArg      = discardSpans <$> textWithout "[]{}|=\n"
> plainTextInLink     = plainTextInTemplate
> urlText             = textWithout "[]| \n"

There's a quirk in Wiki syntax: things that would cause syntax errors just get
output as themselves. So sometimes, some of the characters excluded by
`plainText` are going to appear as plain text, even in contexts where they would
have a meaning -- such as a single closing bracket when two closing brackets
would end a link.

It would be excessive to actually try to simulate MediaWiki's error handling,
but we can write this expression that allows "loose brackets" to be matched
as text:

> oneButNotTwoOf :: [Char] -> Parser Text
> oneButNotTwoOf chars = try (
>   do
>     c <- oneOf chars
>     notFollowedBy (char c)
>     return (T.singleton c)
>   )
>
> looseBracket = notFollowedBy (matchText "{|") >> oneButNotTwoOf "[]{}"

Our `newLine` is like Parsec's built-in `newline` except it returns the newline
character as Text.

> newLine :: Parser Text
> newLine = ((newline <|> crlf) >> return "\n") <?> "newline"
>
> eol :: Parser Text
> eol = (eof >> nop) <|> newLine

Now we can define some spans of text that handle errors, and allow line breaks
where appropriate

> messyText :: Parser Text
> messyText           = textChoices [plainText, looseBracket, symbol "]]", symbol "}}", newLine]
> messyTextLine       = textChoices [plainText, looseBracket, symbol "]]", symbol "}}"]
> messyTextInLink     = textChoices [plainTextInLink, looseBracket, symbol "}}", newLine]
> messyTextInExtLink  = textChoices [plainText, oneButNotTwoOf "[{}", symbol "}}", newLine]
> messyTextInTemplate = textChoices [plainTextInTemplate, looseBracket, symbol "]]", newLine]

Wikitext in general is either some big special environment like a list or a
table -- which we'll handle elsewhere -- or it's made of links, templates, and
miscellaneous text.  We'll parse templates for their meaning below, but in
situations where we don't care about templates, we simply discard their
contents using the `ignoredTemplate` rule.

> wikiTextLine :: Parser Text
> wikiTextLine       = textChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, messyTextLine]       <?> "line of wikitext"
> wikiTextInLink     = textChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, messyTextInLink]     <?> "wikitext inside link"
> wikiTextInExtLink  = textChoices [wikiTable, internalLinkText, ignoredTemplate, messyTextInExtLink]                    <?> "wikitext inside external link"
> wikiTextInTemplate = textChoices [wikiTable, internalLinkText, externalLinkText, ignoredTemplate, messyTextInTemplate] <?> "wikitext inside template"

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

> externalLinkText :: Parser Text
> externalLinkText = try (between bracketAndSchema (string "]") externalLinkContents)
> externalLinkContents = do
>   urlText
>   spaces
>   messyTextInExtLink
> bracketAndSchema = choice (map symbol ["[http://", "[https://", "[ftp://", "[news://", "[irc://", "[mailto:", "[//"])

Internal links have many possible components. In general, they take the form:

    [[namespace:page#section|label]]

The only part that has to be present is the page name. If the label is not
given, then the label is the same as the page.

When parsing internal links, we return just their label. However, other
details of the link are added to the LinkState.

     In:    [[word]]
     Out:   LinkedText [makeLink {page="word"}] "word"

     In:    [[word|this word]]
     Out:   LinkedText [makeLink {page="word"}] "this word"

     In:    [[word#English]]
     Out:   LinkedText [makeLink {page="word", section="English"}] "word"

     In:    [[w:en:Word]]
     Out:   LinkedText [makeLink {namespace="w:en", page="word"}] "Word"

     In:    [[Category:English nouns]]
     Out:   LinkedText [makeLink {namespace="Category", page="English nouns"}] ""


> internalLink :: Parser LinkedText
> internalLink = between (symbol "[[") (symbol "]]") internalLinkContents
> internalLinkContents = do
>   target <- plainTextInLink
>   maybeText <- optionMaybe alternateText
>   let link = (parseLink target) in do
>     case (namespace link) of
>       -- Certain namespaces have special links that make their text disappear
>       "Image"    -> return (linkText link "")
>       "Category" -> return (linkText link "")
>       "File"     -> return (linkText link "")
>       -- If the text didn't disappear, find the text that labels the link
>       _          -> case maybeText of
>         Just text  -> return (linkText link text)
>         -- With no alternate text, the text is the name of the target page
>         Nothing    -> return (linkText link (page link))
>
> internalLinkText :: Parser Text
> internalLinkText = discardLinks <$> internalLink
>
> alternateText :: Parser Text
> alternateText = char '|' >> messyTextInLink
>
> parseLink :: Text -> WikiLink
> parseLink target =
>   WikiLink {namespace=namespace, page=page, section=section}
>   where
>     (namespace, local) = tSplitLast ':' target
>     (page, section) = tSplitFirst '#' local

A LinkedText version of the `textChoices` operator:

> lTextChoices :: [Parser LinkedText] -> Parser LinkedText
> lTextChoices options = lConcatMany (choice options)
>
> lConcatMany :: Parser LinkedText -> Parser LinkedText
> lConcatMany combinator = do
>   parts <- many1 combinator
>   return (concatLinkedText parts)

`linkedWikiText` parses text that may or may not contain links, and returns it
in a LinkedText data structure. `linkedTextFrom` is a more general version that
takes in a parsing expression that it will use to find linked text -- for
example, if there are project-specific templates that should be treated as
links.

> linkedTextFrom :: Parser LinkedText -> Parser LinkedText
> linkedTextFrom expr = concatLinkedText <$> many1 expr

> linkedWikiText :: Parser LinkedText
> linkedWikiText      = linkedTextFrom linkedWikiTextPiece
> linkedWikiTextPiece = internalLink <|> unlinkedWikiText
> unlinkedWikiText    = unlinked <$> textChoices [externalLinkText, ignoredTemplate, messyTextLine]


Lists
-----

Here's a hierarchical data type for describing the contents of lists, which
semantically can contain other lists.

> data ListItem = Item LinkedText
>               | ListHeading LinkedText
>               | BulletList [ListItem]
>               | OrderedList [ListItem]
>               | IndentedList [ListItem]
>               deriving (Show, Eq)

Sometimes we just want the text that the list contains. `extractText`
returns the text of the list items (whatever kind of items they are) separated
by line breaks.

> extractText :: ListItem -> LinkedText
> extractText (Item t) = t
> extractText (ListHeading t) = t
> extractText (BulletList items) = extractTextFromList items
> extractText (OrderedList items) = extractTextFromList items
> extractText (IndentedList items) = extractTextFromList items
>
> extractTextFromList :: [ListItem] -> LinkedText
> extractTextFromList items = joinLinkedText "\n" (map extractText items)

> listItems :: Text -> Parser [ListItem]
> listItems marker = do
>   lookAhead (matchText marker)
>   many1 (listItem marker)
>
> listItem :: Text -> Parser ListItem
> listItem marker = subList marker <|> singleListItem marker

> subList :: Text -> Parser ListItem
> subList marker =   bulletList (T.snoc marker '*')
>                <|> orderedList (T.snoc marker '#')
>                <|> indentedList (T.snoc marker ':')
>                <|> listHeading (T.snoc marker ';')
>
> anyList :: Parser ListItem
> anyList = subList ""
>
> anyListText :: Parser LinkedText
> anyListText = extractText <$> anyList <?> "list"
>
> listHeading :: Text -> Parser ListItem
> listHeading marker = ListHeading <$> listItemContent marker
>
> singleListItem :: Text -> Parser ListItem
> singleListItem marker = Item <$> listItemContent marker
>
> listItemContent :: Text -> Parser LinkedText
> listItemContent marker = do
>   symbol marker
>   optional sameLineSpaces
>   line <- linkedWikiText
>   eol
>   return line
>
> bulletList marker   = BulletList <$> listItems marker
> orderedList marker  = OrderedList <$> listItems marker
> indentedList marker = IndentedList <$> listItems marker
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
> ignoredTemplate :: Parser Text
> ignoredTemplate = do
>   template
>   possiblyEmpty wikiTableDetritus
>   nop

> templateArgs :: Int -> Parser TemplateData
> templateArgs offset = do
>   nameMaybe <- optionMaybe (try templateArgName)
>   case nameMaybe of
>     Just name -> namedArg name offset
>     Nothing -> positionalArg offset
>
> templateArgName :: Parser Text
> templateArgName = do
>   name <- plainTextInArg
>   matchText "="
>   return name
>
> namedArg :: Text -> Int -> Parser TemplateData
> namedArg name offset = do
>   value <- possiblyEmpty wikiTextInTemplate
>   rest <- templateRest offset
>   return (Map.insert name value rest)
>
> positionalArg :: Int -> Parser TemplateData
> positionalArg offset = do
>   value <- possiblyEmpty wikiTextInTemplate
>   rest <- templateRest (offset + 1)
>   return (Map.insert (intToText offset) value rest)
>
> templateRest :: Int -> Parser TemplateData
> templateRest offset = endOfTemplate <|> (matchText "|" >> templateArgs offset)
>
> endOfTemplate = symbol "}}" >> return Map.empty
>
> intToText :: Int -> Text
> intToText = T.pack . show

We can simplify some of this parsing in the case where we are looking for a
*particular* template. We start by expecting two left braces and the name
of the template, then parse the rest of the template as usual.

We set the template name as arg 0, as it would be if we were using the more
general rule for parsing template expressions.

> specificTemplate :: Text -> Parser TemplateData
> specificTemplate name = do
>   symbol (T.append "{{" name)
>   parsed <- templateRest 1
>   return (Map.insert "0" name parsed)


Tables
------

Tables have complex formatting, and thus far we're just going to be skipping
them.

> wikiTable :: Parser Text
> wikiTable = (wikiTableComplete <|> (try wikiTableDetritus >> nop))
>
> wikiTableComplete :: Parser Text
> wikiTableComplete = delimitedSpan "{|" "|}" >> nop

MediaWiki templates can be used in horrifying ways, and one way that they're
sometimes used (particularly on Wikipedia) is to start a table that is then
ended outside the template.

We don't know which templates leave tables hanging open, but when we skip over
such a template, we'll see a bunch of lines starting with |. Lines intended as
text don't normally start with vertical bars. So we can clean up incomplete
tables by skipping over all such lines.

> wikiTableDetritus :: Parser Text
> wikiTableDetritus = do
>   newLine
>   matchText "|"
>   textWithout "\n"
>   try wikiTableDetritus <|> eol


Parsing sections at a time
--------------------------

These functions are designed to take in entire sections of wikitext
(which have already been split by the parser in `Sections.lhs`) and return
the plain text that they contain.

> sectionLinkedText :: Parser LinkedText
> sectionLinkedText = lTextChoices [anyListText, linkedWikiText, unlinked <$> newLine] <?> "section content"

> sectionText :: Parser Text
> sectionText = squishBlankLines <$> discardLinks <$> sectionLinkedText
>
> squishBlankLines :: Text -> Text
> squishBlankLines s = T.unlines (filter isNonEmptyText (T.lines s))
>
> isNonEmptyText :: Text -> Bool
> isNonEmptyText s = (T.length s) > 0


Entry points
------------

Here's a function to be run at the IO level, which takes in Wikitext,
outputs its plain text, and returns nothing.

> outputPlainText :: Text -> IO ()
> outputPlainText input =
>   let sourceName = "(input)" in
>     case parse sectionText sourceName input of
>       Left err -> showError input err
>       Right x -> putStrLn (T.unpack x)
>
> inspectText :: Text -> IO ()
> inspectText input =
>   let sourceName = "(input)" in
>     case parse sectionLinkedText sourceName input of
>       Left err -> showError input err
>       Right (LinkedText links text) -> do
>         print text
>         print links
>
> inspectString :: String -> IO ()
> inspectString input = inspectText $ T.pack input

Showing informative errors:

> showError :: Text -> ParseError -> IO ()
> showError str err =
>   let strLines  = T.lines str
>       errorLine = sourceLine (errorPos err)
>       errorCol  = sourceColumn (errorPos err)
>   in do
>     putStrLn "********"
>     putStr "parse error at "
>     print err
>     putStrLn $ T.unpack (strLines !! (errorLine - 1))
>     putStr (replicate (errorCol - 1) ' ')
>     putStrLn "^"
>     putStrLn "\n"
>     putStrLn $ T.unpack str
>     putStrLn "********"
