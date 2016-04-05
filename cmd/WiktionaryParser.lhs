> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Wiktionary.Base (WiktionaryRel)
> import Text.MediaWiki.Wiktionary.English (enHandlePage)
> import Text.MediaWiki.Wiktionary.French (frHandlePage)
> import Data.ByteString (ByteString)
> import Control.Monad
> import qualified Data.Aeson as Ae
> import qualified Data.ByteString.Lazy.Char8 as LChar8
> import qualified Data.ByteString.Char8 as Char8
> import System.Environment

Language handling
=================

The functions for handling different language Wiktionaries are defined in
separate modules. We take an argument that tells us which language the entries
will be in, so we can delegate to the appropriate handler.

> languageHandler :: String -> ByteString -> ByteString -> [WiktionaryRel]
> languageHandler "en" = enHandlePage
> languageHandler "fr" = frHandlePage
> languageHandler _    = error "unknown language"

Top level
=========

> handlePage :: String -> WikiPage -> IO ()
> handlePage language page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing)
>     (mapM_ (LChar8.putStrLn . Ae.encode)
>            (languageHandler language (pageTitle page) (pageText page)))

> main :: IO ()
> main = do
>   args <- getArgs
>   let language = args !! 0
>   processMediaWikiStdin (handlePage language)

