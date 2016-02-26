The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump, using an extremely minimal XML parser that extracts only the needed
tags. 

A full-featured XML parser would probably not be able to efficiently
take in a large streaming file. A SAX-like XML parser would, but this
module is doing basically the same thing that a SAX-like parser would,
with a specific use case.

This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Text.XML.MediaWikiXML where
> import Text.Parsec
> import Text.Parsec.Char
> import Text.HTML.TagSoup.Entity
> import Data.List
> import Data.Maybe


Data types
==========

> data WikiPage = WikiPage {
>   namespace :: String,
>   title :: String,
>   text :: String,
>   redirect :: Maybe String
> } deriving (Show, Eq)


We'll also define a type expression called Parser. The type expression Parsec
takes three arguments: the input type, the state type, and the output type.
We don't need any state here, so the state type is ().

> type Parser = Parsec String ()

An AList is an association list, that type that shows up in functional
languages, where you map x to y by just putting together a bunch of (x,y)
tuples. Here, in particular, we're mapping strings to strings.

> type AList = [(String,String)]


Lexer rules
===========

`symbol` is a very simple expression that allows us to match a multi-character
string and backtrack if it doesn't actually match. It's defined again in
Text.Wiki.MediaWiki, where it's more fully described, it's just that it's
actually simpler to define it again than to import it.

> symbol = try . string

Define some classes of characters:

> safeChar = noneOf "&;<>\"\'= "
> attrChar = noneOf "&\"<>"
> obligatorySpaces = many1 space

Use Text.HTML.TagSoup to convert entities to the characters they represent.

> entity :: Parser Char
> entity = do
>   char '&'
>   entityName <- many1 safeChar
>   char ';'
>   case lookupEntity entityName of
>     Just (char:[]) -> return char
>     _ -> fail ("Unknown entity " ++ entityName)


Getting text from tags
======================

> attribute :: Parser (String,String)
> attribute = do
>   obligatorySpaces
>   attrName <- many1 safeChar
>   char '='
>   char '"'
>   attrValue <- many (entity <|> attrChar)
>   char '"'
>   return (attrName, attrValue)
>
> selfClose = do
>   spaces
>   symbol "/>"
>   return ""
>
> restOfTag name = do
>   char '>'
>   manyTill (entity <|> anyChar) (symbol ("</" ++ name ++ ">"))

anyTag matches a tag that may or may not contain other tags.

> anyTag :: Parser AList
> anyTag = do
>   try (char '<' >> notFollowedBy (char '/'))
>   tagName <- many1 safeChar
>   many attribute
>   tagValue <- selfClose <|> restOfTag tagName
>   spaces
>   return [(tagName,tagValue)]
>
> openTag :: Parser String
> openTag = do
>   try (char '<' >> notFollowedBy (char '/'))
>   tagName <- many1 safeChar
>   many attribute
>   char '>'
>   spaces
>   return tagName
>
> specificOpenTag :: String -> Parser ()
> specificOpenTag name = do
>   symbol ("<" ++ name)
>   many attribute
>   char '>'
>   spaces
>   return ()
>
> closeTag :: String -> Parser ()
> closeTag name = do
>   symbol ("</" ++ name ++ ">")
>   spaces
>   return ()


Handling for particular tags
----------------------------

> justLookup :: String -> AList -> String
> justLookup key aList = fromJust (lookup key aList)
>
> redirectTag :: Parser AList
> redirectTag = do
>   symbol "<redirect"
>   attrs <- many attribute
>   spaces
>   symbol "/>"
>   case (lookup "title" attrs) of
>     Just title -> return [("redirect",title)]
>     Nothing -> return []
>
> revisionTag = do
>   specificOpenTag "revision"
>   subtags <- many anyTag
>   closeTag "revision"
>   return (concat subtags)
>
> pageTag :: Parser [WikiPage]
> pageTag = do
>   specificOpenTag "page"
>   spaces
>   subtags <- many (revisionTag <|> redirectTag <|> anyTag)
>   closeTag "page"
>   let subtagList = (concat subtags) in
>     return [WikiPage {
>       namespace=(justLookup "ns" subtagList),
>       title=(justLookup "title" subtagList),
>       text=(justLookup "text" subtagList),
>       redirect=(lookup "redirect" subtagList)
>     }]
>
> mediaWikiTag = do
>   specificOpenTag "mediawiki"
>   pages <- many (pageTag <|> (anyTag >> return []))
>   return (concat pages)
>   closeTag "mediawiki"

