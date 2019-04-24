> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)

Top level
=========

> outputPlainTextSection :: WikiSection -> IO ()
> outputPlainTextSection section = do
>   when (length (headings section) > 1) $
>     outputPlainText (lastEx (headings section))
>   outputPlainText (content section)
>
> outputPlainTextPage :: Text -> IO ()
> outputPlainTextPage text =
>   let sections = (parsePageIntoSections text) in
>     mapM_ outputPlainTextSection sections
>
> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     putStrLn (pageTitle page)
>     (outputPlainTextPage (pageText page))
>     putStrLn "--------"

> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

