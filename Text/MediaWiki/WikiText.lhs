`Text.MediaWiki.WikiText`: parse the WikiText format
====================================================

> {-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}

This is the core of Wikiparsec: a set of parsing rules for handling the Wikitext
format.

These rules can be used to extract structured information or unstructured text
from Wikitext, depending on the task at hand. For example, the `anyList` parser
will parse this Wikitext:

    # item 1
    #* item 2a
    #* item 2b
    # item 3

into this data structure:

    OrderedList [Item "item 1", BulletList [Item "item 2a", Item "item 2b"], Item "item 3"]

In contrast, the `articleSectionWikitext` parser will extract the plain text
from this paragraph of Wikitext:

    [[File:Ainola yard.jpg|thumb|left|Ainola, Sibelius's home from 1904 until
    his death|alt=A white house of north European appearance with an orange
    tiled roof, surrounded by trees]]
    Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous [[Grand
    Duchy of Finland|grand duchy]] within the [[Russian Empire]] having earlier
    been under Swedish control for many centuries. The country remained divided
    between a culturally dominant Swedish-speaking minority, to which the
    Sibelius family belonged, and a more nationalistically-minded
    Finnish-speaking, or "[[Fennoman movement|Fennoman]]" majority. In about
    1889 Sibelius met his future wife, [[Aino Sibelius|Aino Järnefelt]], who
    came from a staunch Fennoman family.  Sibelius's association with the
    Järnefelts helped to awaken and develop his own nationalism; in 1892, the
    year of his marriage to Aino, he completed his first overtly nationalistic
    work, the symphonic suite ''[[Kullervo (Sibelius)|Kullervo]]''. Through the
    1890s, as Russian control over the duchy grew increasingly oppressive,
    Sibelius produced a series of works reflecting Finnish resistance to
    foreign rule, culminating in the tone poem ''[[Finlandia]]''.

Resulting in this text:

    Ainola, Sibelius's home from 1904 until his death
    Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous grand
    duchy within the Russian Empire having earlier been under Swedish control
    for many centuries. The country remained divided between a culturally
    dominant Swedish-speaking minority, to which the Sibelius family belonged,
    and a more nationalistically-minded Finnish-speaking, or "Fennoman"
    majority. In about 1889 Sibelius met his future wife, Aino Järnefelt, who
    came from a staunch Fennoman family. Sibelius's association with the
    Järnefelts helped to awaken and develop his own nationalism; in 1892, the
    year of his marriage to Aino, he completed his first overtly nationalistic
    work, the symphonic suite Kullervo. Through the 1890s, as Russian control
    over the duchy grew increasingly oppressive, Sibelius produced a series of
    works reflecting Finnish resistance to foreign rule, culminating in the
    tone poem Finlandia.

Setup
-----

Import the WikiPrelude and Attoparsec. Hide a couple of function names that
we'll want to redefine.

> module Text.MediaWiki.WikiText where
> import WikiPrelude hiding (try)
> import Data.Attoparsec.Text hiding (endOfLine)
> import Data.Attoparsec.Combinator

Pull in some string-manipulating utilities that are defined elsewhere in
this package:

> import Text.SplitUtils (splitFirst, splitLast)

Our useful tools for defining parse rules:

> import Text.MediaWiki.ParseTools (nop, appendChar, textWith, textWithout,
>   skipChars, textChoices, notFollowedByChar, possiblyEmpty,
>   delimitedSpan, optionMaybe)

Handling templates:

> import Text.MediaWiki.Templates (Template, TemplateProc, ignoreTemplates,
>   evalTemplate)

Marking up text:

> import Text.MediaWiki.AnnotatedText

Spans of text
-------------

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

These closing brackets and braces are just string literals, but we assign
them a more meaningful name for the purpose of debugging using `<?>`.

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

Any parse rule that can handle templates needs to be passed a `TemplateProc`,
specifying what to do when it encounters a template. The desired behavior
varies by the use case. Sometimes we want to convert certain templates into
links or annotations, in which case we'll pass in a procedure that defines how
to do that. Sometimes we just want to throw all templates out, in which case
`ignoreTemplates` is the `TemplateProc` that we want.

So `wikiTextLine` and its siblings are not Attoparsec parser combinators, per
se; instead, they're functions that you apply to a `TemplateProc` to get a
parser combinator. `wikiTextLine ignoreTemplates` is a parser combinator.

When we give `internalLinkText` as an option, we need to provide an extra
boolean argument for whether it's allowed to recurse and match
`internalLinkText` again within. (See `internalLink`, below.)

> wikiTextLine :: TemplateProc -> Parser Text
> wikiTextLine tproc        =
>   textChoices [wikiTable, internalLinkText True tproc, externalLinkText, templateText tproc, messyTextLine]          <?> "line of wikitext"
> wikiTextInLink tproc      =
>   textChoices [internalLinkText True tproc, externalLinkText, templateText tproc, messyTextInLink]                   <?> "wikitext inside link"
> wikiTextAtEndOfLink tproc =
>   textChoices [wikiTable, internalLinkText False tproc, externalLinkText, templateText tproc, messyTextAtEndOfLink]  <?> "wikitext at end of link"
> wikiTextInTemplate tproc  =
>   textChoices [internalLinkText True tproc, externalLinkText, templateText tproc, messyTextInTemplate]               <?> "wikitext inside template"

Wiki syntax for links
---------------------

External links appear in single brackets. They contain a URL, a space, and
the text that labels the link, such as:

    [http://www.americanscientist.org/authors/detail/david-van-tassel David Van Tassel]

We would like to extract just the visible text from that link, which is
"David Van Tassel".

External links can have no text, in which case they just get an arbitrary
number as their text, which we'll disregard. There's also a type of external
link that is just a bare URL in the text. Its effect on the text is exactly
the same as if it weren't a link, so we can disregard that case.

The following rules extract the text of an external link.

We start by matching a single left bracket (under the assumption that, if there
were two left brackets, they would have been matched by the `internalLinkText`
rule first). After that, we parse the interior of the link. If that fails, we
just return the left bracket as plain text, simulating MediaWiki's error
handling.

> externalLinkText :: Parser Text
> externalLinkText = do
>   char '['
>   externalLinkMatch <|> return "["

After the left bracket, we look for a URL schema, the rest of the URL, and a
possible label on the link.

> externalLinkMatch = do
>   schema
>   urlText
>   externalLinkLabelOrEnd
>
> schema = choice (map string ["http://", "https://", "ftp://", "news://", "irc://", "mailto:", "//"]) <?> "external link schema"
> externalLinkLabelOrEnd = externalLinkEnd <|> externalLinkLabel

After the URL, the link might end, in which case there's no label and we want
to throw it out. Or there can be a label, in which case we want to get its text,
which could include formatting and could require error handling,
using the `messyTextInExtLink` rule.

The `*>` operator means "parse the first thing, throw it out, and parse the
second thing for its value". `<*` is the same but gets the value from the first
thing. The value that counts is the one being pointed to. These operators let
us write simple combinations of parsers without do-notation.

> externalLinkEnd = char ']' *> return ""
> externalLinkLabel = skipSpace *> messyTextInExtLink <* externalLinkEnd

Internal links have many possible components. In general, they take the form:

    [[namespace:page#section|label]]

The only part that has to be present is the page name. If the label is not
given, then the label is the same as the page.

We represent the result of parsing a link as AnnotatedText, where the label is
the text and the other properties are annotations. Some examples:

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

It is possible, in link syntax, for the text of the link to contain another
link. In fact, this is a common occurrence in image captions, because images
are a kind of link.

However, there's never a reason for this to recurse more than twice. If there
are more than two levels of nested links, something is wrong. And these cases
where something is wrong could require exponential time to parse, because the
error case for links requires backtracking.

So the first argument to `internalLink` is a boolean for whether it is allowed
to recurse and match `internalLink` again internally. We pass this boolean on
to the `alternateText` rule.

> internalLink :: Bool -> TemplateProc -> Parser AnnotatedText
> internalLink recurse tproc = do
>   string "[["
>   target <- plainTextInLink
>   let {
>     (page, section) = splitFirst "#" target ;
>     (namespace, local) = splitLast ":" page
>   } in do
>     maybeText <- optionMaybe (alternateText recurse tproc)
>     let {
>       text = fromMaybe local maybeText ;
>       link = makeLink namespace local section text ;
>       annotated = annotate [link] text
>     } in do
>        string "]]"
>        return annotated

The label of a link can be made of Wikitext and can even include templates or,
in the case of image captions, other links.  When we encounter a label that's
different from the link target, we need to parse it as Wikitext, including
handling templates with a `TemplateProc`.

The `recurse` flag determines whether this rule is allowed to match links.

> alternateText :: Bool -> TemplateProc -> Parser Text
> alternateText recurse tproc = do
>   char '|'
>   text <- if recurse
>             then wikiTextAtEndOfLink tproc
>             else messyTextAtEndOfLink
>   return (extractLinkText text)
>

In some cases, we only want the text of the link, in which case we operate on
the parse result with `getText`.

> internalLinkText :: Bool -> TemplateProc -> Parser Text
> internalLinkText recurse tproc = getText <$> internalLink recurse tproc

There are complicated syntaxes on MediaWiki that look like internal links,
particularly the Image: or File: syntax, which can have multiple
vertical-bar-separated parts, and assigns properties such as alternate text to
an image, as well as a plain-text caption that has no special syntax to
introduce it -- it seems to be determined by process of elimination.

Our best guess at which part of the syntax is the caption is the last one without an equals
sign. If all parts have an equals sign, perhaps because there's an innocent equals sign in
a link's text, then we return the last part.

For example, in this image syntax:

    [[File:Ainola yard.jpg|300px|Ainola, Sibelius's home from 1904 until his death|alt=A white house of north European appearance with an orange tiled roof, surrounded by trees]]

the text we want to extract is:

    Ainola, Sibelius's home from 1904 until his death

On the other hand, images aren't usually in the flow of text; they're usually
set off to the side using "thumb" as one of the arguments. In that case, we
just want to skip them.

> extractLinkText :: Text -> Text
> extractLinkText text =
>   if isPrefixOf "thumb" text
>     then ""
>     else
>       -- Get the part of a link that's most likely to be its displayed text.
>       -- If there are many parts to choose from, prefer the ones without
>       -- equals signs (which may be image metadata, for example).
>       let parts      = splitOn "|" text
>           noEquals t = not (isInfixOf "=" t)
>           priority   = parts ⊕ (filter noEquals parts)
>       -- We use MinLen functions to convince the type system that there will
>       -- be a "last" element. We know there is one because, even if our priority
>       -- order is empty, we stick "" on the front as a last resort.
>       in last (ncons "" priority)

`annotatedWikiText` parses text that may or may not contain links or templates,
and returns it in an AnnotatedText data structure.

> annotatedWikiText :: TemplateProc -> Parser AnnotatedText
> annotatedWikiText tproc = concat <$> many1 (annotatedWikiTextPiece tproc)
> annotatedWikiTextPiece tproc = internalLink True tproc <|> templateValue tproc <|> simpleWikiTextPiece
> simpleWikiTextPiece = annotFromText <$> choice [wikiTable, externalLinkText, messyTextLine]

Sometimes there's extra syntax going on, so we need to exclude specific
characters from the wikitext.

When this rule is used, it will consume any character except the listed ones
when they appear in plain text. For that reason, the line break `\n` often
belongs in `exclude`.

> annotatedWikiTextWithout :: [Char] -> TemplateProc -> Parser AnnotatedText
> annotatedWikiTextWithout exclude tproc =
>   mconcat <$> many' (
>     internalLink True tproc
>     <|> templateValue tproc
>     <|> annotFromText <$> (textWithout (exclude ⊕ "\n[]{}"))
>     )


Wiki syntax for lists
---------------------

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

If we're extracting lines from a list and encounter a sublist, use
`concat` to flatten the results of `extractTextLines` into a single list.

> extractTextLinesFromList :: [ListItem] -> [AnnotatedText]
> extractTextLinesFromList items = concat (map extractTextLines items)

`extractText` concatenates the result of `extractTextLines` into a single
AnnotatedText, with the list item texts separated by line breaks.

> extractText :: ListItem -> AnnotatedText
> extractText = joinAnnotatedLines . extractTextLines

In some cases (such as Wiktionary definition lists), we want to extract only
the texts from the top level of a list, not from the sublists. Instead of
recursing, we go on to the `extractItem` function, which returns a single
element for a leaf and nothing for a list.

> extractTopLevel :: ListItem -> [AnnotatedText]
> extractTopLevel (Item item) = [item]
> extractTopLevel (ListHeading item) = []
> extractTopLevel (BulletList items) = extractTopLevelFromList items
> extractTopLevel (OrderedList items) = extractTopLevelFromList items
> extractTopLevel (IndentedList items) = extractTopLevelFromList items
>
> extractTopLevelFromList :: [ListItem] -> [AnnotatedText]
> extractTopLevelFromList items = concat (map extractItem items)
>
> extractItem :: ListItem -> [AnnotatedText]
> extractItem (Item item) = [item]
> extractItem _ = []

If what we expect to see is a list of links to other entries, sometimes we want
to be stingier than that, getting just the first link from each item. (So far,
we don't need to apply this across an entire list, just one item at a time.)

> extractFirstLink :: ListItem -> [AnnotatedText]
> extractFirstLink (Item item) = [limitAnnotationToFirstLink item]
> extractFirstLink _ = []
>
> limitAnnotationToFirstLink :: AnnotatedText -> AnnotatedText
> limitAnnotationToFirstLink (AnnotatedText annos text) =
>   AnnotatedText (filterForFirstLink annos []) text

`filterForFirstLink` is used in implementing `limitAnnotationToFirstLink`,
scanning the annotation list of an AnnotatedText and keeping only those up to
the first that contains a "page" item.

> filterForFirstLink :: [Annotation] -> [Annotation] -> [Annotation]
> filterForFirstLink (thisAnnotation:rest) seen =
>   if (member "page" thisAnnotation)
>     then reverse (thisAnnotation:seen)
>     else filterForFirstLink rest (thisAnnotation:seen)
>
> filterForFirstLink [] seen = reverse seen

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


Wiki syntax for templates
-------------------------

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
>   name <- stripSpaces <$> plainTextInArg
>   string "="
>   return name
>
> namedArg :: TemplateProc -> Text -> Int -> Parser Template
> namedArg tproc name offset = do
>   value <- stripSpaces <$> possiblyEmpty (wikiTextInTemplate tproc)
>   rest <- templateRest tproc offset
>   return (insertMap name value rest)
>
> positionalArg :: TemplateProc -> Int -> Parser Template
> positionalArg tproc offset = do
>   value <- stripSpaces <$> possiblyEmpty (wikiTextInTemplate tproc)
>   rest <- templateRest tproc (offset + 1)
>   let name = (intToText offset) in
>     return (insertMap name value rest)
>
> templateRest :: TemplateProc -> Int -> Parser Template
> templateRest tproc offset = endOfTemplate <|> (string "|" >> templateArgs tproc offset)
>
> endOfTemplate :: Parser Template
> endOfTemplate = string "}}" >> return ø
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


Wiki syntax for tables
----------------------

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

To get links instead of text:

> sectionLinks :: TemplateProc -> Parser [Annotation]
> sectionLinks tproc = getLinks <$> sectionAnnotated tproc

> sectionArticleLinks :: TemplateProc -> Parser [Annotation]
> sectionArticleLinks tproc = getArticleLinks <$> sectionAnnotated tproc


Entry points
------------

Parse all the text of a section.

> parseEntireSection = parseOnly (sectionText ignoreTemplates <* endOfInput)
> parseEntireSectionLinks = parseOnly (sectionLinks ignoreTemplates <* endOfInput)

Here's a function to be run at the IO level, which takes in Wikitext,
outputs its plain text, and returns nothing.

> outputPlainText :: Text -> IO ()
> outputPlainText input =
>    case parseEntireSection input of
>      Left err -> showError input err
>      Right x -> putStrLn x

`inspectText` shows the parsed plain text as well as its annotations.

> inspectText :: Text -> IO ()
> inspectText input =
>   case parseOnly (sectionAnnotated ignoreTemplates <* endOfInput) input of
>     Left err -> showError input err
>     Right (AnnotatedText annos text) -> do
>       putStrLn text
>       print annos

`inspectString` is designed to be usable from the REPL, where OverloadedStrings
may not be available: it takes in a built-in String (a type that is generally not
used in this codebase), and converts it to a Text so it can be inspected.

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
