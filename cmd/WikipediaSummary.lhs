> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
> import WikiPrelude
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText, parseEntireSection, showError)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import Data.Text.ICU.Replace (replaceAll)
> import Text.SplitUtils (removeParentheticals)

Top level
=========

Some specific functionality to clean up extracted text. It particularly
removes parentheticals, which in the lead paragraphs tend to be insipid
minutiae that cargo-cultishly imitate the way Britannica articles begin.
After that, it removes extra spaces, including before punctuation.

> cleanup :: Text -> Text
> cleanup =
>   (replaceAll "\\s+" " ") .
>   removeParentheticals

We throw out summaries that end in a colon (as they're probably unhelpful
disambiguation lines), and empty summaries.

> okayText :: Text -> Bool
> okayText text = (
>   (length text) > 0    
>   && not (isSuffixOf ":" (stripSpaces text))
>   )

Okay, now all the imperative code that gets text onto standard out, in various
formats. Haskell devs will hate this code and I kinda do too.

> showArticleTabular :: Text -> Text -> IO ()
> showArticleTabular title text = let ctext = (cleanup text) in do
>   when (okayText ctext) $ do
>     putStr title
>     putStr "\t"
>     putStrLn ctext
>
> outputTabularFirstSection :: Text -> Text -> IO ()
> outputTabularFirstSection title text =
>   let sections = (parsePageIntoSections text)
>       Just firstSection = headMay sections
>   in case parseEntireSection (content firstSection) of
>     Left err -> return ()
>     Right parsed -> showArticleTabular title parsed
>
> outputPlainTextArticle :: Text -> Text -> IO ()
> outputPlainTextArticle title text = do
>   putStr "\n\n<article> "
>   putStrLn title
>   putStr "\n"
>   let sections = impureNonNull (parsePageIntoSections text)
>       firstSection = head sections
>       restSections = tail sections
>     in do
>       outputPlainFirstSection firstSection
>       mapM_ outputPlainTextSection restSections
>
> outputPlainFirstSection :: WikiSection -> IO ()
> outputPlainFirstSection section = do
>   case parseEntireSection (content section) of
>     Left err -> return ()
>     Right parsed -> putStrLn (removeParentheticals parsed)
>
> outputPlainTextSection :: WikiSection -> IO ()
> outputPlainTextSection section = do
>   let headingLvl = length (headings section)
>   let heading = last (impureNonNull (headings section))
>   putStr "\n"
>   putStr (replicate headingLvl '#')
>   putStr " "
>   putStrLn heading
>   putStr "\n"
>   case parseEntireSection (content section) of
>     Left err -> return ()
>     Right parsed -> putStrLn parsed
>
> handlePage :: Bool -> WikiPage -> IO ()
> handlePage fullArticles page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing) $ do
>     if fullArticles
>       then (outputPlainTextArticle (pageTitle page) (pageText page))
>       else (outputTabularFirstSection (pageTitle page) (pageText page))
>
> main :: IO ()
> main = do
>   args <- getArgs
>   processMediaWikiStdin (handlePage ("--articles" ∈ args))
>   
