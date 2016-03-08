> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump. This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Text.MediaWiki.XML where
> import qualified Data.ByteString.UTF8 as UTF8
> import qualified Data.ByteString.Lazy as ByteString

XML and text decoding:

> import Data.Text (Text)
> import Data.XML.Types
> import Data.Maybe
> import Text.XML.Stream.Parse

Decompression:

> import Data.Conduit.BZlib

Crazy streaming framework:

> import Data.Conduit
> import qualified Data.Conduit.List as CL
> import qualified Data.Conduit.Binary as Binary
> import Control.Monad.Trans.Resource
> import Control.Monad.Trans.Class (lift)
> import Control.Monad (void)

The HTML processor that we'll run on the output:

> import Text.MediaWiki.HTML (extractWikiTextFromHTML)

Data types
==========

> data WikiPage = WikiPage {
>   pageNamespace :: Text,
>   pageTitle :: Text,
>   pageText :: Text
> } deriving (Show, Eq)
>
> type WikiConduit u = ConduitM Event WikiPage (ResourceT IO) u

An AList is an association list, that type that shows up in functional
languages, where you map x to y by just putting together a bunch of (x,y)
tuples. Here, in particular, we're mapping text names to text values.

> type AItem = (Text,Text)
> type AList = [AItem]
>
> justLookup :: Text -> AList -> Text
> justLookup key aList = fromJust (lookup key aList)

Top level
=========

I honestly do not know what most of this code does. The general outline of it
comes from
http://haddock.stackage.org/lts-5.4/xml-conduit-1.3.3.1/Text-XML-Stream-Parse.html.

The `runResourceT` function is something about freeing up I/O resources in case
of exceptions, which is not actually a problem I was going to encounter, but
xml-conduit makes me solve it anyway.

> processMediaWikiDump :: String -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiDump filename sink = runResourceT $
>   Binary.sourceFile filename $= bunzip2 $$ parseBytes def =$ (void parseMediaWikiXML) =$ CL.mapM_ (lift . sink)

Parsing some XML
================

I really apologize for the relative lack of types. The xml-conduit API is bad
(everyone on StackOverflow agrees with this, but there's no alternative), and
the functions it works with are nearly impossible to write correct type
signatures for. I tried for hours.

> ns :: Text -> Name
> ns name = Name { nameLocalName=name, nameNamespace=Just "http://www.mediawiki.org/xml/export-0.10/", namePrefix=Nothing }

> matchSingleChild name = do
>   contents <- manyIgnore (tagIgnoreAttrs (ns name) content)
>                          (ignoreTree (/= (ns name)))
>   case contents of
>     found:_ -> return found
>     _       -> error ("Couldn't find child tag: " ++ show name)
>
> matchChild name = tagIgnoreAttrs (ns name) (itemize name)
>
> itemize name = do
>   value <- content
>   return (name,value)
>
> matchRevision = tagNoAttr (ns "revision") matchText
> matchText = do
>   text <- matchSingleChild "text"
>   return ("text",text)

> parseMediaWikiXML = tagIgnoreAttrs (ns "mediawiki") parsePages
> parsePages = manyYield' parsePage
>
> parsePage = tagNoAttr (ns "page") parsePageContent
>
> parsePageContent :: WikiConduit WikiPage
> parsePageContent =
>   let parsers = (choose [matchRevision, matchChild "ns", matchChild "title"]) in do
>     assoc <- manyIgnore parsers ignoreAllTrees
>     return $ WikiPage {
>       pageNamespace=(justLookup "ns" assoc),
>       pageTitle=(justLookup "title" assoc),
>       pageText=extractWikiTextFromHTML (justLookup "text" assoc)
>     }

