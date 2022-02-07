> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (parseEntireSectionLinks, showError)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import Text.MediaWiki.AnnotatedText (Annotation)

Top level
=========

> showLinks :: Text -> [Annotation] -> IO ()
> showLinks title = mapM_ (showLink title)
>
> showLink :: Text -> Annotation -> IO ()
> showLink title link = do
>   putStr title
>   putStr "\t"
>   putStr (get "page" link)
>   putStr "\t"
>   putStrLn (get "text" link)
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

