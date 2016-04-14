> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

The purpose of this module is to extract Wikitext data from a MediaWiki XML
dump. This module won't parse the Wikitext itself; that's a job for
Text.Wiki.MediaWiki.

> module Text.MediaWiki.XML where
> import WikiPrelude

XML and text decoding:

> import qualified Text.XML.Expat.SAX as SAX

The HTML processor that we'll run on the output:

> import Text.MediaWiki.HTML (extractWikiTextFromHTML)

Data types
==========

When we extract sub-tags of the page, we'll represent them as an association
list from ByteStrings to ByteStrings. (An association list, which is just a
list of pairs, will work fine as a mapping here because the mapping is small:
it will only ever contain 4 elements.)

> type ByteStringAssoc = [(ByteString, ByteString)]
>
> justLookup :: ByteString -> ByteStringAssoc -> ByteString
> justLookup key aList = fromMaybe (error ("Missing tag: " ++ (show key))) (lookup key aList)

Once we've found the subtags, then we'll wrap them up as a record called
WikiPage, making sure to decode the values as Text.

> data WikiPage = WikiPage {
>   pageNamespace :: Text,
>   pageTitle :: Text,
>   pageText :: Text,
>   pageRedirect :: Maybe Text
> } deriving (Show, Eq)
>
> makeWikiPage :: ByteStringAssoc -> WikiPage
> makeWikiPage subtags = WikiPage {
>    pageNamespace = decodeUtf8 (justLookup "ns" subtags),
>    pageTitle = decodeUtf8 (justLookup "title" subtags),
>    pageText = extractWikiTextFromHTML (justLookup "text" subtags),
>    pageRedirect = decodeUtf8 <$> lookup "redirect" subtags
> }

Top level
=========

> processMediaWikiContent :: LByteString -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiContent content sink = do
>   let events = SAX.parse SAX.defaultParseOptions content
>   mapM_ sink (findPageTags events)
>
> processMediaWikiDump :: FilePath -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiDump filename sink = do
>   content <- readFile filename
>   processMediaWikiContent content sink
>
> processMediaWikiStream :: Handle -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiStream source sink = do
>   content <- hGetContents source
>   processMediaWikiContent content sink
>
> processMediaWikiStdin = processMediaWikiStream stdin

Parsing some XML
================

> findPageTags = handleEventStream [] []

> handleEventStream :: ByteStringAssoc -> [ByteString] -> [SAX.SAXEvent ByteString ByteString] -> [WikiPage]
> handleEventStream subtags chunks [] = []
> handleEventStream subtags chunks ((SAX.StartElement "page" attrs):rest) = handleEventStream [] [] rest
> handleEventStream subtags chunks ((SAX.StartElement "redirect" attrs):rest) =
>   let title = justLookup "title" attrs
>   in handleEventStream (("redirect",title):subtags) [] rest
> handleEventStream subtags chunks ((SAX.StartElement elt attrs):rest) = handleEventStream subtags [] rest
> handleEventStream subtags chunks ((SAX.EndElement "page"):rest) = ((makeWikiPage subtags):(handleEventStream [] [] rest))
> handleEventStream subtags chunks ((SAX.EndElement elt):rest) = handleEventStream ((elt, mconcat (reverse chunks)):subtags) [] rest
> handleEventStream subtags chunks ((SAX.CharacterData t):rest) = handleEventStream subtags (t:chunks) rest
> handleEventStream subtags chunks ((SAX.FailDocument (SAX.XMLParseError err loc)):rest) =
>   error ("XML parse error: " ++ err ++ " at " ++ (show loc))
> handleEventStream subtags chunks (misc:rest) = error ("Can't handle element: " ++ show misc)
