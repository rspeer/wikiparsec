This is a command-line interface that simply takes in MediaWiki XML content
(TODO) on standard input, and parses it to find the plain text, which it
writes to standard output.

> import Text.Wiki.MediaWiki
>
> main :: IO ()
> main = do
>   input <- getContents
>   case parse pageText "(stdin)" input of
>     Left err -> do
>       putStrLn "parse error at"
>       print err
>     Right x -> do
>       putStrLn x
