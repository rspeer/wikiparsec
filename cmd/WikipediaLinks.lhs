This is a script that extracts links from Wikipedia articles in a tabular form.
The three columns are:

- source: the title of the page the link appears on
- text: the visible text of the link
- target: the title of the page the link points to

Redirect pages count as a single link, whose text is the page being redirected
from and whose target is the page being redirected to. The source is the string
`#REDIRECT`.

Some examples from early in the English Wikipedia dump:

    #REDIRECT       AccessibleComputing     Computer accessibility
    Anarchism       political philosophy    political philosophy
    Anarchism       movement                Political movement
    Alabama         Southeastern            Southeastern United States
    Alabama         United States           United States
    #REDIRECT       Action Film             Action film
    #REDIRECT       Ameboid stage           Amoeba
    ASCII           tab                     Tab key


> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Data.Text.ICU.Replace (replaceAll)
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (parseEntireSectionLinks, showError)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import Text.MediaWiki.AnnotatedText (Annotation)

Outputting links in tabular form
================================

Take in a page title and a list of extracted annotations, and output them as
links in tabular form.

> showLinks :: Text -> [Annotation] -> IO ()
> showLinks title = mapM_ (showLink title)
>
> showLink :: Text -> Annotation -> IO ()
> showLink title link = do
>   putStr title
>   putStr "\t"
>   putStr (replaceAll "\n" " " (get "text" link))
>   putStr "\t"
>   when ((get "namespace" link) /= "") (putStr (get "namespace" link))
>   putStrLn (get "page" link)

Similar to showLinks, but for the special case of redirect links.

> showRedirectLinks :: Text -> [Annotation] -> IO ()
> showRedirectLinks title = mapM_ (showRedirectLink title)
>
> showRedirectLink :: Text -> Annotation -> IO ()
> showRedirectLink title link = do
>   putStr title
>   putStr "\t"
>   putStr "#REDIRECT"
>   putStr "\t"
>   putStrLn (get "page" link)

I've found it's helpful downstream for every page to conceptually include
a link to itself, under its own title.

> showSelfLink :: Text -> IO ()
> showSelfLink title = do
>   putStr title
>   putStr "\t"
>   putStr title
>   putStr "\t"
>   putStrLn title

Extracting links from pages
===========================

Scan through an article and output its links, a section at a time.

> showSectionLinks title section =
>   case parseEntireSectionLinks (content section) of
>     Left err -> return ()
>     Right parsed -> showLinks title parsed
>
> showPageLinks :: Text -> Text -> IO ()
> showPageLinks title text =
>   let sections = (parsePageIntoSections text)
>   in mapM_ (showSectionLinks title) sections

The similar mechanism for the case of a redirect page.

> showRedirectPageLinks :: Text -> Text -> IO ()
> showRedirectPageLinks title text =
>   case parseEntireSectionLinks text of
>     Left err -> return ()
>     Right parsed -> showRedirectLinks title parsed

Determine whether a page is a redirect, and handle it with handleArticlePage or
handleRedirectPage as appropriate.

> handlePage :: WikiPage -> IO ()
> handlePage page =
>   case (pageRedirect page) of
>     (Just redirect) -> handleRedirectPage redirect page
>     Nothing -> handleArticlePage page
>
> handleRedirectPage :: Text -> WikiPage -> IO ()
> handleRedirectPage redirect page = do
>   when (pageNamespace page == "0") $ do
>     showRedirectPageLinks (pageTitle page) (pageText page)
>
> handleArticlePage page = do
>   when (pageNamespace page == "0") $ do
>     showSelfLink (pageTitle page)
>     showPageLinks (pageTitle page) (pageText page)

Top level
=========

> main :: IO ()
> main = do
>   processMediaWikiStdin handlePage

