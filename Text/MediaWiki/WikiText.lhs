> {-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}

Setup
=====

To parse the mess that is Wiktionary, we make use of Attoparsec, a
well-regarded parser-combinator library for Haskell.

> module Text.MediaWiki.WikiText where
> import WikiPrelude hiding (try)
> import Data.Attoparsec.Text hiding (endOfLine)
> import Data.Attoparsec.Combinator

Pull in some string-manipulating utilities that are defined elsewhere in
this package:

> import Text.MediaWiki.SplitUtils (splitFirst, splitLast)

Some common shorthand for defining parse rules:

> import Text.MediaWiki.ParseTools (nop, appendChar, textWith, textWithout,
>   skipChars, textChoices, concatMany, notFollowedByChar, possiblyEmpty,
>   delimitedSpan, optionMaybe)

Handling templates:

> import Text.MediaWiki.Templates (Template, TemplateProc, ignoreTemplates,
>   evalTemplate)

Marking up text:

> import Text.MediaWiki.AnnotatedText

Spans of text
=============

Some formatting allows whitespace as long as it stays on the same line --
for example, the whitespace around headings and after list bullets.

> optionalSameLineSpaces :: Parser ()
> optionalSameLineSpaces = skipChars " \t"

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
apostrophes, by chaining it through the `discardSpans` function.

> discardSpans :: Text -> Text
> discardSpans = (replace "''" "") . (replace "'''" "")

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
but we can write some expressions that allows various combinations of brackets
to be matched as plain text:

- `]]` in a context where we're not expecting to end an internal link
- `}}` in a context where we're not expecting to end a template
- A single `{` not followed by `|`
- A single `]`
- A single `}`

We don't handle single opening brackets here because those often introduce
external links. Instead, if the external link parser fails to parse a link,
it'll just return the bracket as is.

> looseBracket :: Parser Text
> looseBracket = oneButNotTwoOf "]}" <|> looseOpeningBrace
>
> looseOpeningBrace :: Parser Text
> looseOpeningBrace = do
>   char '{'
>   notFollowedByChar '{'
>   notFollowedByChar '|'
>   return "{"
>
> oneButNotTwoOf :: [Char] -> Parser Text
> oneButNotTwoOf chars = try (
>   do
>     c <- satisfy (inClass chars)
>     notFollowedByChar c
>     return (singleton c)
>   )
>
> extraneousCloseBrackets = string "]]" <?> "junk closing brackets"
> extraneousCloseBraces   = string "}}" <?> "junk closing braces"

Attoparsec has a misnamed combinator named `endOfLine`. I say it's misnamed
because it matches an actual line break, but doesn't match at the end of
the input.

We define `newLine` here to match the line break (I don't think we need to
handle `\r` when our input comes from XML), and `endOfLine` to also include
the end of input.

> newLine :: Parser Text
> newLine = string "\n"
>
> endOfLine :: Parser Text
> endOfLine = (endOfInput >> nop) <|> newLine <?> "end of line"

Now we can define some spans of text that handle errors, and allow line breaks
where appropriate.

> messyText :: Parser Text
> messyText            = textChoices [plainText, looseBracket, extraneousCloseBrackets, extraneousCloseBraces, newLine] <?> "plain text"
> messyTextLine        = textChoices [plainText, looseBracket, extraneousCloseBrackets, extraneousCloseBraces]          <?> "line of plain text"
> messyTextInLink      = textChoices [plainTextInLink, looseBracket, extraneousCloseBraces, newLine]                    <?> "plain text inside link"
> messyTextInExtLink   = textChoices [plainText, oneButNotTwoOf "[{}", extraneousCloseBraces, newLine]                  <?> "plain text inside external link"
> messyTextAtEndOfLink = textChoices [plainText, looseBracket, extraneousCloseBraces, newLine]                          <?> "plain text at end of link"
> messyTextInTemplate  = textChoices [plainTextInTemplate, looseBracket, extraneousCloseBrackets, newLine]              <?> "plain text inside template"

Wikitext in general is either some big special environment like a list or a
table -- which we'll handle elsewhere -- or it's made of links, templates, and
miscellaneous text. When we encounter a template, we have to turn it into an
AnnotatedText value and then a plain Text value, which the `templateText`
rule does.

> wikiTextLine :: TemplateProc -> Parser Text
> wikiTextLine tproc        = textChoices [wikiTable, internalLinkText tproc, externalLinkText, templateText tproc, messyTextLine]       <?> "line of wikitext"
> wikiTextInLink tproc      = textChoices [internalLinkText tproc, externalLinkText, templateText tproc, messyTextInLink]                <?> "wikitext inside link"
> wikiTextAtEndOfLink tproc = textChoices [wikiTable, internalLinkText tproc, externalLinkText, templateText tproc, messyTextAtEndOfLink]<?> "wikitext at end of link"
> wikiTextInTemplate tproc  = textChoices [internalLinkText tproc, externalLinkText, templateText tproc, messyTextInTemplate]            <?> "wikitext inside template"

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

The following rules extract the text of an external link.

> externalLinkText :: Parser Text
> externalLinkText = do
>   char '['
>   externalLinkMatch <|> return "["
>
> externalLinkMatch = do
>   schema
>   urlText
>   externalLinkLabelOrEnd
>
> schema = choice (map string ["http://", "https://", "ftp://", "news://", "irc://", "mailto:", "//"]) <?> "external link schema"
> externalLinkLabelOrEnd = externalLinkEnd <|> externalLinkLabel
> externalLinkEnd = char ']' *> return ""
> externalLinkLabel = skipSpace *> messyTextInExtLink <* externalLinkEnd

Internal links have many possible components. In general, they take the form:

    [[namespace:page#section|label]]

The only part that has to be present is the page name. If the label is not
given, then the label is the same as the page.

When parsing internal links, we return just their label. However, other
details of the link are added to the LinkState.

     In:    [[word]]
     Out:   AnnotatedText [mapFromList [("page", "word")]] "word"

     In:    [[word|this word]]
     Out:   AnnotatedText [mapFromList [("page", "word")]] "this word"

     In:    [[word#English]]
     Out:   AnnotatedText [mapFromList [("page", "word"), ("section", "English")]] "word"

     In:    [[w:en:Word]]
     Out:   AnnotatedText [mapFromList [("namespace", "w:en"), ("page", "word")]] "Word"

     In:    [[Category:English nouns]]
     Out:   AnnotatedText [mapFromList [("namespace", "Category"), ("page", "English nouns")]] "English nouns"


> internalLink :: TemplateProc -> Parser AnnotatedText
> internalLink tproc = do
>   string "[["
>   target <- plainTextInLink
>   maybeText <- optionMaybe (alternateText tproc)
>   let {
>     link      = parseLink target;
>     annotated = case maybeText of
>                   Just text -> annotate [link] text
>                   Nothing   -> annotate [link] (get "page" link)
>   } in do
>        string "]]"
>        return annotated
>
> internalLinkText :: TemplateProc -> Parser Text
> internalLinkText tproc = getText <$> internalLink tproc

There are complicated syntaxes on MediaWiki that look like internal links,
particularly the Image: or File: syntax, which can have multiple
vertical-bar-separated parts, and assigns properties such as alternate text to
an image, as well as a plain-text caption that has no special syntax to
introduce it -- it seems to be determined by process of elimination.

Our best guess at which part of the syntax is the caption is the last one without an equals
sign. If all parts have an equals sign, perhaps because there's an innocent equals sign in
a link's text, then we return the last part.

For example, in this image syntax:

    [[File:Ainola yard.jpg|thumb|left|Ainola, Sibelius's home from 1904 until his death|alt=A white house of north European appearance with an orange tiled roof, surrounded by trees]]

the text we want to extract is:

    Ainola, Sibelius's home from 1904 until his death

> alternateText :: TemplateProc -> Parser Text
> alternateText tproc = do
>   char '|'
>   text <- wikiTextAtEndOfLink tproc
>   return (extractLinkText text)
>
> extractLinkText :: Text -> Text
> extractLinkText text =
>   -- Get the part of a link that's most likely to be its displayed text.
>   -- If there are many parts to choose from, prefer the ones without
>   -- equals signs (which may be image metadata, for example).
>   let parts      = splitOn "|" text
>       noEquals t = not (isInfixOf "=" t)
>       priority   = parts <> (filter noEquals parts)
>   -- We use MinLen functions to convince the type system that there will
>   -- be a "last" element. We know there is one because, even if our priority
>   -- order is empty, we stick "" on the front as a last resort.
>   in last (mlcons "" (toMinLenZero priority))
>
> parseLink :: Text -> Annotation
> parseLink target =
>   makeLink namespace page section
>   where
>     (namespace, local) = splitLast ":" target
>     (page, section) = splitFirst "#" local

`annotatedWikiText` parses text that may or may not contain links or templates,
and returns it in an AnnotatedText data structure.

> annotatedWikiText :: TemplateProc -> Parser AnnotatedText
> annotatedWikiText tproc = concat <$> many1 (annotatedWikiTextPiece tproc)
> annotatedWikiTextPiece tproc = internalLink tproc <|> templateValue tproc <|> simpleWikiTextPiece
> simpleWikiTextPiece = annotFromText <$> choice [wikiTable, externalLinkText, messyTextLine]

Sometimes there's extra syntax going on, so we need to exclude specific
characters from the wikitext.

When this rule is used, it will consume any character except the listed ones
when they appear in plain text. For that reason, "\n" often belongs in
`exclude`.

> annotatedWikiTextWithout :: [Char] -> TemplateProc -> Parser AnnotatedText
> annotatedWikiTextWithout exclude tproc =
>   mconcat <$> many (
>     internalLink tproc
>     <|> templateValue tproc
>     <|> annotFromText <$> (textWithout (exclude <> "\n[]{}"))
>     )


Lists
-----

Here's a hierarchical data type for describing the contents of lists, which
semantically can contain other lists.

> data ListItem = Item AnnotatedText
>               | ListHeading AnnotatedText
>               | BulletList [ListItem]
>               | OrderedList [ListItem]
>               | IndentedList [ListItem]
>               deriving (Show, Eq)

Sometimes we just want the text that the list contains. `extractTextLines`
returns the texts of the list items (whatever kind of items they are) as
a list of AnnotatedTexts.

> extractTextLines :: ListItem -> [AnnotatedText]
> extractTextLines (Item t) = [t]
> extractTextLines (ListHeading t) = [t]
> extractTextLines (BulletList items) = extractTextLinesFromList items
> extractTextLines (OrderedList items) = extractTextLinesFromList items
> extractTextLines (IndentedList items) = extractTextLinesFromList items
>
> extractTextLinesFromList :: [ListItem] -> [AnnotatedText]
> extractTextLinesFromList items = concat (map extractTextLines items)

`extractText` concatenates the result of `extractTextLines` into a single
AnnotatedText, with the list item texts separated by line breaks.

> extractText :: ListItem -> AnnotatedText
> extractText = joinAnnotatedLines . extractTextLines

In some cases (such as Wiktionary definition lists), we want to extract only
the texts from the top level of a list, not from the sublists.

> extractTopLevel :: ListItem -> [AnnotatedText]
> extractTopLevel (Item item) = [item]
> extractTopLevel (ListHeading item) = []
> extractTopLevel (BulletList items) = extractTopLevelFromList items
> extractTopLevel (OrderedList items) = extractTopLevelFromList items
> extractTopLevel (IndentedList items) = extractTopLevelFromList items
>
> extractTopLevelFromList :: [ListItem] -> [AnnotatedText]
> extractTopLevelFromList items = concat (map extractItemsFromList items)
>
> extractItemsFromList :: ListItem -> [AnnotatedText]
> extractItemsFromList (Item item) = [item]
> extractItemsFromList _ = []

And here are the rules for parsing lists:

> listItems :: TemplateProc -> Text -> Parser [ListItem]
> listItems tproc marker = do
>   lookAhead (string marker)
>   many1 (listItem tproc marker)
>
> listItem :: TemplateProc -> Text -> Parser ListItem
> listItem tproc marker = subList tproc marker <|> singleListItem tproc marker

> subList :: TemplateProc -> Text -> Parser ListItem
> subList tproc marker = bulletList tproc (appendChar marker '*')
>                    <|> orderedList tproc (appendChar marker '#')
>                    <|> indentedList tproc (appendChar marker ':')
>                    <|> listHeading tproc (appendChar marker ';')
>
> anyList :: TemplateProc -> Parser ListItem
> anyList tproc = subList tproc ""
>
> anyListText :: TemplateProc -> Parser AnnotatedText
> anyListText tproc = extractText <$> anyList tproc <?> "list"
>
> listHeading :: TemplateProc -> Text -> Parser ListItem
> listHeading tproc marker = ListHeading <$> listItemContent tproc marker
>
> singleListItem :: TemplateProc -> Text -> Parser ListItem
> singleListItem tproc marker = Item <$> listItemContent tproc marker
>
> listItemContent :: TemplateProc -> Text -> Parser AnnotatedText
> listItemContent tproc marker = do
>   string marker
>   optionalSameLineSpaces
>   line <- annotatedWikiText tproc
>   endOfLine
>   return line
>
> bulletList tproc marker   = BulletList <$> listItems tproc marker
> orderedList tproc marker  = OrderedList <$> listItems tproc marker
> indentedList tproc marker = IndentedList <$> listItems tproc marker
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

> template :: TemplateProc -> Parser Template
> template tproc = string "{{" >> (templateArgs tproc 0)
>
> templateValue :: TemplateProc -> Parser AnnotatedText
> templateValue tproc = (evalTemplate tproc) <$> template tproc
>
> templateText :: TemplateProc -> Parser Text
> templateText tproc = getText <$> templateValue tproc

A point that might be confusing: the following parsers take a TemplateProc as
their first argument not because they'll be using it to evaluate this template
we're parsing, but because the template might contain nested templates that
have to be evaluated.

> templateArgs :: TemplateProc -> Int -> Parser Template
> templateArgs tproc offset = do
>   nameMaybe <- optionMaybe (try templateArgName)
>   case nameMaybe of
>     Just name -> namedArg tproc name offset
>     Nothing -> positionalArg tproc offset
>
> templateArgName :: Parser Text
> templateArgName = do
>   name <- plainTextInArg
>   string "="
>   return name
>
> namedArg :: TemplateProc -> Text -> Int -> Parser Template
> namedArg tproc name offset = do
>   value <- possiblyEmpty (wikiTextInTemplate tproc)
>   rest <- templateRest tproc offset
>   return (insertMap name value rest)
>
> positionalArg :: TemplateProc -> Int -> Parser Template
> positionalArg tproc offset = do
>   value <- possiblyEmpty (wikiTextInTemplate tproc)
>   rest <- templateRest tproc (offset + 1)
>   let name = (intToText offset) in
>     return (insertMap name value rest)
>
> templateRest :: TemplateProc -> Int -> Parser Template
> templateRest tproc offset = endOfTemplate <|> (string "|" >> templateArgs tproc offset)
>
> endOfTemplate :: Parser Template
> endOfTemplate = string "}}" >> return mempty
>
> intToText :: Int -> Text
> intToText = pack . show

We can simplify some of this parsing in the case where we are looking for a
*particular* template. We start by expecting two left braces and the name
of the template, then parse the rest of the template as usual.

We set the template name as arg 0, as it would be if we were using the more
general rule for parsing template expressions.

> specificTemplate :: TemplateProc -> Text -> Parser Template
> specificTemplate tproc name = do
>   string (mappend "{{" name)
>   parsed <- templateRest tproc 1
>   return (("0",name):parsed)


Tables
------

Tables have complex formatting, and thus far we're just going to be skipping
them.

> wikiTable :: Parser Text
> wikiTable = wikiTableComplete
>
> wikiTableComplete :: Parser Text
> wikiTableComplete = delimitedSpan "{|" "|}" >> nop


Parsing sections at a time
--------------------------

These functions are designed to take in entire sections of wikitext
(which have already been split by the parser in `Sections.lhs`) and return
the plain text that they contain.

> sectionAnnotated :: TemplateProc -> Parser AnnotatedText
> sectionAnnotated tproc =
>   transformA squishBlankLines <$>
>     possiblyEmpty (textChoices [anyListText tproc, annotatedWikiText tproc, annotFromText <$> newLine]) <?> "section content"
>
> sectionText :: TemplateProc -> Parser Text
> sectionText tproc = getText <$> sectionAnnotated tproc
>
> squishBlankLines :: Text -> Text
> squishBlankLines s = unlines (filter isMeaningfulLine (lines s))
>
> isMeaningfulLine :: Text -> Bool
> isMeaningfulLine s = (length s) > 0 && not (isPrefixOf "|" s) && not (isPrefixOf "!" s) && not (isDirective s)
> isDirective s = (isPrefixOf "__" s) && (isSuffixOf "__" s)


Entry points
------------

Here's a function to be run at the IO level, which takes in Wikitext,
outputs its plain text, and returns nothing.

> outputPlainText :: Text -> IO ()
> outputPlainText input =
>    case parseOnly (sectionText ignoreTemplates <* endOfInput) input of
>     Left err -> showError input err
>     Right x -> putStrLn x
>
> inspectText :: Text -> IO ()
> inspectText input =
>   case parseOnly (sectionAnnotated ignoreTemplates <* endOfInput) input of
>     Left err -> showError input err
>     Right (AnnotatedText links text) -> do
>       putStrLn text
>       print links
>
> inspectString :: String -> IO ()
> inspectString input = inspectText $ pack input

Showing informative errors:

> showError :: Text -> String -> IO ()
> showError str err = do
>   putStrLn "********"
>   putStr "parse error:"
>   print err
>   putStrLn str
>   putStrLn "********"
