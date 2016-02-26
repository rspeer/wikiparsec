> {-# LANGUAGE OverloadedStrings #-}

The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump. This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Main where
> import qualified Data.ByteString.UTF8 as UTF8
> import qualified Data.ByteString.Lazy as ByteString

Imports for parsing XML in a stream:

> import Data.Conduit
> import qualified Data.Conduit.List as CL
> import Data.Text (Text, unpack)
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


Top level
=========

This is based on http://haddock.stackage.org/lts-5.4/xml-conduit-1.3.3.1/Text-XML-Stream-Parse.html.

> main :: IO ()
> main = do
>   stdin <- ByteString.getContents
>   runResourceT $
>     parseLBS def stdin $$ parseMediaWiki =$ outputSink
>       where outputSink = CL.mapM_ (lift . print)


Parsing some XML
================

> parseMediaWiki = void (tagIgnoreAttrs "mediawiki" parsePages)
> parsePages = manyYield' parsePage
> parsePage = tagNoAttr "page" findTitle
> findTitle = do
>   many (ignoreAnyTreeName ["revision", "ns", "id", "redirect", "restrictions"])
>   tagNoAttr "title" content

