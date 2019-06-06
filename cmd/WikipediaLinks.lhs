> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (parseEntireSectionLinks, showError)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)

Top level
=========

> showLinks :: Text -> [Text] -> IO ()
> showLinks title = mapM_ (showLink title)
>
> showLink :: Text -> Text -> IO ()
> showLink title link = do
>   putStr title
>   putStr "\t"
>   putStrLn link
>
> showSectionLinks title section =
>   case parseEntireSectionLinks (content section) of
>     Left err -> return ()
>     Right parsed -> showLinks title parsed
>
> showPageLinks :: Text -> Text -> IO ()
> showPageLinks title text =
>   let sections = (parsePageIntoSections text)
>   in mapM_ (showSectionLinks title) sections
>
> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     showPageLinks (pageTitle page) (pageText page)
>
> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

