> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText, parseEntireSection, showError)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)

Top level
=========

> showArticle :: Text -> Text -> IO ()
> showArticle title text = do
>   putStr title
>   putStr "\t"
>   putStrLn (replace "\n" " " text)
>
> outputPlainTextFirstSection :: Text -> Text -> IO ()
> outputPlainTextFirstSection title text =
>   let sections = (parsePageIntoSections text)
>       Just firstSection = headMay sections
>   in case parseEntireSection (content firstSection) of
>     Left err -> return ()
>     Right parsed -> showArticle title parsed
>
> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     outputPlainTextFirstSection (pageTitle page) (pageText page)
>
> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

