> {-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as Char8
> import Control.Monad

Top level
=========

> outputPlainTextSection :: WikiSection -> IO ()
> outputPlainTextSection section = do
>   when (length (headings section) > 1) $
>     outputPlainText (last (headings section))
>   outputPlainText (content section)
>
> outputPlainTextPage :: ByteString -> IO ()
> outputPlainTextPage text =
>   case (parsePageIntoSections text) of
>     Left err -> print err
>     Right sections -> mapM_ outputPlainTextSection sections
>
> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     Char8.putStrLn (pageTitle page)
>     (outputPlainTextPage (pageText page))

> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

