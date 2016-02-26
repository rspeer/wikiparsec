> {-# LANGUAGE OverloadedStrings #-}

The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump. This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Text.XML.MediaWikiXMLConduit where
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
> import Text.XML.Stream.Parse

Some exceedingly general monad bullshit that the documentation uses:

> import Control.Monad.Trans.Resource
> import Control.Monad.Trans.Class (lift)
> import Control.Monad (void)

Data types
==========

> data WikiPage = WikiPage {
>   namespace :: String,
>   title :: String,
>   text :: String,
>   redirect :: Maybe String
> } deriving (Show, Eq)

An AList is an association list, that type that shows up in functional
languages, where you map x to y by just putting together a bunch of (x,y)
tuples. Here, in particular, we're mapping strings to strings.

> type AList = [(String,String)]

> type ConduitStream t u = ConduitM Event t (ResourceT IO) u

Top level
=========

This is based on http://haddock.stackage.org/lts-5.4/xml-conduit-1.3.3.1/Text-XML-Stream-Parse.html.

> wiktSource = Binary.sourceFile "/wobbly/data/wiktionary/enwiktionary-20151201-pages-articles.xml.bz2"
> outputSink = CL.mapM_ (lift . print)
>
> main :: IO ()
> main = runResourceT $ wiktSource $= bunzip2 $$ parseBytes def =$ parseMediaWiki =$ outputSink

Parsing some XML
================

> ns :: Text -> Name
> ns name = Name { nameLocalName=name, nameNamespace=Just "http://www.mediawiki.org/xml/export-0.10/", namePrefix=Nothing }
>
> parseMediaWiki :: ConduitStream WikiPage ()
> parseMediaWiki = void (tagIgnoreAttrs (ns "mediawiki") parsePages)
> parsePages = manyYield' parsePage
> parsePage = tagNoAttr (ns "page") findTitle
>
> findTitle :: ConduitStream WikiPage WikiPage
> findTitle = do
>   titles <- manyIgnore (tagNoAttr (ns "title") extractName) (ignoreAnyTreeName (map ns ["revision", "ns", "id", "redirect", "restrictions"]))
>   return (head titles)

   many (ignoreAnyTreeName (map ns ["revision", "ns", "id", "redirect", "restrictions"]))
   tagNoAttr (ns "title") extractName
   many (ignoreAnyTreeName (map ns ["revision", "ns", "id", "redirect", "restrictions"]))

> extractName = do
>   name <- content
>   return (WikiPage {namespace="0", text="", title=(unpack name), redirect=Nothing})
