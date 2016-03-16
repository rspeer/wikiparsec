> import Text.MediaWiki.XML (processMediaWikiDump, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import Control.Monad
> import System.Environment

Top level
=========

> outputPlainTextSection :: WikiSection -> IO ()
> outputPlainTextSection section = do
>   when (length (headings section) > 1) $
>     outputPlainText (T.strip (last (headings section)))
>   outputPlainText (content section)
>
> outputPlainTextPage :: T.Text -> IO ()
> outputPlainTextPage text =
>   case (parsePageIntoSections text) of
>     Left err -> print err
>     Right sections -> mapM_ outputPlainTextSection sections
>
> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == (T.pack "0") && pageRedirect page == Nothing) $ do
>     TIO.putStrLn (pageTitle page)
>     (outputPlainTextPage (pageText page))

> main :: IO ()
> main = do
>   args <- getArgs
>   processMediaWikiDump (args !! 0) handlePage

