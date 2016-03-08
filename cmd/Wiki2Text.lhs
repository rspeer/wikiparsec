> import Text.MediaWiki.XML (processMediaWikiDump, WikiPage,
>                                pageNamespace, pageTitle, pageText)
> import Text.MediaWiki.WikiText (outputPlainText)
> import Data.Text (unpack, pack)
> import Control.Monad

Top level
=========

> handlePage :: WikiPage -> IO ()
> handlePage page = do
>   when (pageNamespace page == (pack "0"))
>     (outputPlainText (unpack (pageText page)))

> main :: IO ()
> main = processMediaWikiDump "/wobbly/data/wordfreq/raw-input/wikipedia/enwiki-20141208-pages-articles.xml.bz2" handlePage

main = processMediaWikiDump "/wobbly/data/wiktionary/enwiktionary-20151201-pages-articles.xml.bz2" handlePage
