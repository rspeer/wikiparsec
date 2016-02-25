This is a command-line interface that simply takes in MediaWiki XML content
(TODO) on standard input, and parses it to find the plain text, which it
writes to standard output.

> import Text.Wiki.MediaWiki
> import Data.Maybe
> import Text.XML.HXT.Core
> import Text.XML.HXT.XPath.Arrows
>
> -- screw this, I understand what SAX is for now
>
> readStdin = readDocument [ withValidate no
>                          , withInputEncoding utf8
>                          , withTagSoup yes
>                          , withCheckNamespaces no
>                          , withSubstDTDEntities no
>                          , withSubstHTMLEntities yes
>                          ] ""
>
> -- I don't actually understand this type signature, but what it means is
> -- that this function is a filter that takes in XmlTrees and outputs
> -- Strings, and works with the "arrow" combinator.
> extractPlainTextFromElement :: ArrowXml a => a XmlTree String
> extractPlainTextFromElement = getText >>> extractPlainText
>
> extractPlainText :: String -> [String]
> extractPlainText wikitext = maybeToList (parse pageText "(stdin)" wikitext)
>
> -- This really is a filter: we want to select only XmlTrees that are in
> -- namespace 0.
> reasonableArticle :: ArrowXml a => a XmlTree XmlTree
> reasonableArticle = (getChildren >>> hasName 
>
> main :: IO ()
> main = do
>   -- I'm just working from examples at https://wiki.haskell.org/HXT.
>   runX $
>     readStdin
>     >>> deep (isElem >>> hasName "page")
>     >>> 
>
>
>   input <- readDocument [] ""
>   input <- getContents
>   case parse pageText "(stdin)" input of
>     Left err -> do
>       putStrLn "parse error at"
>       print err
>     Right x -> do
>       putStrLn x
