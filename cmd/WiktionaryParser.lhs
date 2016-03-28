> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Wiktionary.English (enHandlePage)
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as Char8
> import Control.Monad
> import qualified Data.Aeson as Ae
> import qualified Data.ByteString.Lazy.Char8 as LChar8

Top level
=========

> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing)
>     (mapM_ (LChar8.putStrLn . Ae.encode) (enHandlePage (pageTitle page) (pageText page)))

> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

