> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction, DeriveGeneric #-}

> import WikiPrelude
> import Data.LanguageType
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.Wiktionary.Base (WiktionaryFact)
> import Text.MediaWiki.Wiktionary.English (enParseWiktionary)
> import Text.MediaWiki.Wiktionary.French (frParseWiktionary)
> import Text.MediaWiki.Wiktionary.German (deParseWiktionary)
> import Data.Aeson (ToJSON)
> import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder)

Language handling
=================

The functions for handling different language Wiktionaries are defined in
separate modules. We take an argument that tells us which language the entries
will be in, so we can delegate to the appropriate handler.

> languageHandler :: Language -> Text -> Text -> [WiktionaryFact]
> languageHandler "en"  = enParseWiktionary
> languageHandler "fr"  = frParseWiktionary
> languageHandler "de"  = deParseWiktionary
> languageHandler other = error ("unknown language: " <> (cs (fromLanguage other)))

Page info
=========

PageInfo is just a struct that can be JSON-encoded to let readers know when we
start handling a new page, so that they can transform the data one page at a
time.

> data PageInfo = PageInfo {
>   title :: Text,
>   language :: Language
> } deriving (Eq, Show, Generic)
>
> instance ToJSON PageInfo

JSON printing
=============

We'll use Aeson's `encodePretty'` function so that we can output keys in a
consistent order for reproducible builds.

However, that function also inserts newlines, which would ruin the "streaming
JSON" format we want to output, in which there's exactly one object per line.
So we also need to post-process the result to remove newlines.

> customOrder = keyOrder ["rel", "language", "text", "pos"]
> customConfig = Config { confIndent = 0, confCompare = customOrder <> compare }
>
> customEncode :: ToJSON a => a -> Text
> customEncode obj = replace "\n" " " $ cs $ encodeMultiline obj
>
> encodeMultiline :: ToJSON a => a -> LByteString
> encodeMultiline = encodePretty' customConfig

Top level
=========

> handlePage :: Language -> WikiPage -> IO ()
> handlePage language page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     let pageInfo = PageInfo { title=(pageTitle page), language=language }
>     (println . customEncode) pageInfo
>     (mapM_ (println . customEncode)
>            (languageHandler language (pageTitle page) (pageText page)))

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     (language:_) -> processMediaWikiStdin (handlePage (toLanguage language))
>     _            -> error "Please give a language code as a command-line argument"

