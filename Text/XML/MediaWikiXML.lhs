> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump. This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Text.XML.MediaWikiXML where
> import qualified Data.ByteString.UTF8 as UTF8
> import qualified Data.ByteString.Lazy as ByteString

Decompression:

> import Data.Conduit.BZlib (bunzip2)

Imports for parsing XML in a stream:

> import Data.Conduit
> import qualified Data.Conduit.List as CL
> import qualified Data.Conduit.Binary as Binary
> import Data.Text (Text, unpack, pack)
> import Data.XML.Types
> import Data.Maybe
> import Text.XML.Stream.Parse

Some exceedingly general monad bullshit that the documentation uses:

> import Control.Monad.Trans.Resource
> import Control.Monad.Trans.Class (lift)
> import Control.Monad (void)

Data types
==========

> data WikiPage = WikiPage {
>   namespace :: Text,
>   title :: Text,
>   text :: Text
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

This is based on http://haddock.stackage.org/lts-5.4/xml-conduit-1.3.3.1/Text-XML-Stream-Parse.html.

> -- Take the function that does useful work, and hit it with a monad hammer until it fits into stdout
> outputify processor = CL.mapM_ (lift . print . processor)
>
> processMediaWikiDump :: (Show a) => String -> (WikiPage -> a) -> IO ()
> processMediaWikiDump filename processor = runResourceT $
>   Binary.sourceFile filename $= bunzip2 $$ parseBytes def =$ (void parseMediaWiki) =$ outputify processor
>
> main = processMediaWikiDump "/wobbly/data/wiktionary/enwiktionary-20151201-pages-articles.xml.bz2" id

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
>     _       -> monadThrow (XmlException ("Couldn't find child tag: " ++ show name) Nothing)
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

> parseMediaWiki = tagIgnoreAttrs (ns "mediawiki") parsePages
> parsePages = manyYield' parsePage
>
> parsePage = tagNoAttr (ns "page") parsePageContent
>
> parsePageContent :: WikiConduit WikiPage
> parsePageContent =
>   let parsers = (choose [matchRevision, matchChild "ns", matchChild "title"]) in do
>     assoc <- manyIgnore parsers ignoreAllTrees
>     return $ WikiPage {
>       namespace=(justLookup "ns" assoc),
>       title=(justLookup "title" assoc),
>       text=(justLookup "text" assoc)
>     }

