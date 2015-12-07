\section{Setup}

To parse the mess that is Wiktionary, we make use of Parsec, perhaps the
best-regarded parser-combinator library I've ever encountered.

Parsec is explicitly designed around the way Haskell works. I wouldn't
normally be using Haskell, but it does seem like the right tool for the job.

> module Text.Wiki.MediaWiki where
> import Text.Parsec hiding (parse, parseTest)
> import Text.Parsec.Char
> import Control.Monad.Identity

Pull in some string-manipulating utilities that are defined elsewhere in
this package:

> import Text.SplitUtils

We're going to need to make use of Haskell's functional mapping type,
Data.Map, to represent the contents of templates.

> import Data.Map (Map)
> import qualified Data.Map as Map
> type TemplateData = Map String String

We'll also define a type expression called Parser. The type expression Parsec
takes three arguments: the input type, the state type, and the output type.

For most expressions in this file, the input type will be String and the state
type will be LinkState, a relatively short-lived state that keeps track of
links that have appeared in the text. Let's gloss over LinkState for now
and define it at the end of the module.

All we have left to specify is the output type, which can vary, so we won't
fill in that argument.

> type Parser = Parsec String LinkState


\section{Parser-making expressions}

The awkward thing about LL parsing is that you can consume part of a string,
fail to match the rest of it, and be unable to backtrack. When we match a
string, we usually want it to be an all-or-nothing thing. At the cost of a bit
of efficiency, we'll use the {\tt symbol} expression for multi-character
strings, which wraps the {\tt string} combinator in {\tt try} so it can
backtrack.

> symbol = try . string

This is similar to the {\tt symbol} that's defined in Parsec's token-based
parse rules, but we're not importing those because they don't coexist with
significant whitespace.

In various situations, we'll want to parse ``some amount of arbitrary text
without syntax in it''. But what this means is, unfortunately, different in
different situations. Sometimes line breaks are allowed. Sometimes unmatched
brackets and braces are allowed. And so on.

To make this easier, we'll define {\tt textChoices}, which takes a list of
expressions we're allowed to parse, tries all of them in that priority order,
and concatenates together their results.

> textChoices :: [Parser String] -> Parser String
> textChoices options = concatMany (choice (map try options))
>
> concatMany :: Parser String -> Parser String
> concatMany combinator = do
>   parts <- many combinator
>   return (concat parts)


\section{Spans of text}

I forget exactly why, but I think we're going to need an expression that
allows whitespace as long as it stays on the same line. (FIXME check this)
If we allowed linebreaks, we could just use {\tt spaces} from
Text.Parsec.Char.

Wikitext is whitespace-sensitive. (FIXME describe more)

> sameLineSpaces :: Parser ()
> sameLineSpaces = skipMany (oneOf " \t")

The ``ignored'' expression matches HTML tags and comments and throws them
away.

> ignored :: Parser String
> ignored = do
>   skipMany1 ignoredItem
>   return ""
>
> ignoredItem = try htmlComment <|> try htmlTag
>
> htmlComment :: Parser String
> htmlComment = do
>   string "<!--"
>   manyTill anyChar (symbol "-->")
>
> htmlTag :: Parser String
> htmlTag = do
>   char '<'
>   manyTill anyChar (char '>')

Our most reusable expression for miscellaneous text, {\tt basicText}, matches
characters that aren't involved in any kind of Wiki syntax.

> basicText :: Parser String
> basicText = many1 (noneOf "[]{}|<>:=\n")

However, there's a quirk: things that would cause syntax errors just get output
as themselves. So sometimes, some of the above characters are going to appear
as plain text, even in contexts where they would have a meaning -- such as
a single closing bracket when two closing brackets would end a link.

It would be excessive to actually try to simulate MediaWiki's error handling,
but we can write this expression that allows ``loose brackets'' to be matched
as text:

> looseBracket :: Parser String
> looseBracket = do
>   notFollowedBy internalLink
>   notFollowedBy externalLink
>   notFollowedBy template
>   bracket <- oneOf "[]{}"
>   notFollowedBy (char bracket)
>   return [bracket]

Wikitext in general is made of HTML, links, templates, and miscellaneous text.
We'll parse templates for their meaning below, but in situations where we don't
care about templates, we simply discard their contents using the {\tt
ignoredTemplate} rule.

> wikiTextLine :: Parser String
> wikiTextLine = textChoices [ignored, internalLink, externalLink, ignoredTemplate, looseBracket, textLine]
> wikiText = textChoices [ignored, internalLink, externalLink, ignoredTemplate, looseBracket, textLine, eol]
> textLine = many1 (noneOf "[]{}<>\n")
> eol = string "\n"


\section{Wiki syntax items}

External links appear in single brackets. They contain a URL, a space, and
the text that labels the link, such as:

\begin{verbatim}
In:  [http://www.americanscientist.org/authors/detail/david-van-tassel David Van Tassel]
Out: "David Van Tassel"
\end{verbatim}

External links can have no text, in which case they just get an arbitrary
number as their text, which we'll disregard. There's also a type of external
link that is just a bare URL in the text. Its effect on the text is exactly
the same as if it weren't a link, so we can disregard that case.

The following rules extract the text of an external link, as both ``between''
and ``do'' return their last argument.

> externalLink :: Parser String
> externalLink = between (string "[") (string "]") externalLinkContents
> externalLinkContents = do
>   schema
>   urlPath
>   spaces
>   linkTitle
> schema = choice (map string ["http://", "https://", "ftp://", "news://", "irc://", "mailto:", "//"])
> urlPath = many1 (noneOf "[]{}<>| ")
> linkTitle = textChoices [ignored, linkText]
> linkText = many1 (noneOf "[]{}|<>")

Internal links have many possible components. In general, they take the form:

>--   [[namespace:page#section|label]]

The only part that has to be present is the page name. If the label is not
given, then the label is the same as the page.

When parsing internal links, we return just their label. However, other
details of the link are added to the LinkState.

\begin{verbatim}

     In: [[word]]
    Out: "word"
  State: [makeLink {page="word"}]

     In: [[word|this word]]
    Out: "this word"
  State: [makeLink {page="word"}]

     In: [[word#English]]
    Out: "word"
  State: [makeLink {page="word", section="English"}]

     In: [[w:en:Word]]
    Out: "word"
  State: [makeLink {namespace="w:en", page="word"}]

     In: [[Category:English nouns]]
    Out: ""
  State: [makeLink {namespace="Category", page="English nouns"}]

\end{verbatim}

> internalLink :: Parser String
> internalLink = between (symbol "[[") (symbol "]]") internalLinkContents
> internalLinkContents = do
>   target <- linkTarget
>   maybeText <- optionMaybe alternateText
>   let link = (parseLink target) in do
>     updateState (addLink link)
>     case (namespace link) of
>       -- Certain namespaces have special links that make their text disappear
>       "Image"    -> return ""
>       "Category" -> return ""
>       "File"     -> return ""
>       -- If the text didn't disappear, find the text that labels the link
>       _          -> case maybeText of
>         Just text  -> return text
>         Nothing    -> return target
>
> linkTarget :: Parser String
> linkTarget = many1 (noneOf "[]{}|<>\n")
>
> alternateText = string "|" >> linkText
>
> parseLink :: String -> WikiLink
> parseLink target =
>   WikiLink {namespace=namespace, page=page, section=section}
>   where
>     (namespace, local) = splitLast ':' target
>     (page, section) = splitFirst '#' local

\subsection{Templates}

A simple template looks like this:

>--   {{archaic}}

More complex templates take arguments, such as this translation into French:

>--   {{t+|fr|exemple|m}}

And very complex templates can have both positional and named arguments:

>--   {{t|ja|例え|tr=[[たとえ]], tatoe}}

Some templates are more detailed versions of internal links. Some are metadata
that we can simply ignore. The ultimate semantics of a template can depend both
on its contents and the section in which it appears.

But first, we need to recognize the syntax of templates.

> template :: Parser TemplateData
> template = symbol "{{" >> (templateArgs 0)
>
> ignoredTemplate :: Parser String
> ignoredTemplate = template >> return ""

> templateArgs :: Integer -> Parser TemplateData
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
> namedArg :: String -> Integer -> Parser TemplateData
> namedArg name offset = do
>   value <- wikiTextArg
>   rest <- templateRest offset
>   return (Map.insert name value rest)
>
> positionalArg :: Integer -> Parser TemplateData
> positionalArg offset = do
>   value <- wikiTextArg
>   rest <- templateRest (offset + 1)
>   return (Map.insert (show offset) value rest)
>
> templateRest :: Integer -> Parser TemplateData
> templateRest offset = endOfTemplate <|> (string "|" >> templateArgs offset)
>
> endOfTemplate = symbol "}}" >> return Map.empty
>
> wikiTextArg = textChoices [ignored, internalLink, externalLink, looseBracket, textArg]
> textArg = many1 (noneOf "[]{}<>|")


\section{Keeping track of links}

As our parser runs, it will be collecting links in a value that we call a
LinkState.

> type LinkState = [WikiLink]
>
> data WikiLink = WikiLink {
>   namespace :: String,
>   page :: String,
>   section :: String
> } deriving (Show, Eq)

The {\tt makeLink} constructor allows creating a WikiLink where the
values default to the empty string.

> makeLink = WikiLink {namespace="", page="", section=""}

Here are some functions that apply to LinkStates:

> newState :: LinkState
> newState = []
>
> resetState :: LinkState -> LinkState
> resetState ps = []
>
> addLink :: WikiLink -> LinkState -> LinkState
> addLink = (:)

And here's a variant of the wikiText parser combinator that returns the list
of WikiLinks that it accumulates:

> wikiTextLinks :: Parser LinkState
> wikiTextLinks = do
>   text <- wikiText
>   getState


\section{Entry points}

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
> parse parser = runParser parser newState

