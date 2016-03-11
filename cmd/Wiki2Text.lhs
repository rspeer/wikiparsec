> import Text.MediaWiki.XML (processMediaWikiDump, WikiPage,
>                                pageNamespace, pageTitle, pageText)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Text.MediaWiki.Sections (parsePageIntoSections, WikiSection, headings, content)
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import Control.Monad

Top level
=========

> outputPlainTextSection :: WikiSection -> IO ()
> outputPlainTextSection section = do
>   TIO.putStrLn (last (headings section))
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
>   when (pageNamespace page == (T.pack "0"))
>     (outputPlainTextPage (pageText page))

> main :: IO ()
> main = processMediaWikiDump "/wobbly/data/wiktionary/onepage.xml.bz2" handlePage

main = processMediaWikiDump "/wobbly/data/wordfreq/raw-input/wikipedia/enwiki-20141208-pages-articles.xml.bz2" handlePage
main = processMediaWikiDump "/wobbly/data/wiktionary/enwiktionary-20151201-pages-articles.xml.bz2" handlePage
