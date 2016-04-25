> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

> import WikiPrelude
> import Data.LanguageType
> import Text.MediaWiki.XML (processMediaWikiStdin, WikiPage,
>                            pageNamespace, pageTitle, pageText, pageRedirect)
> import Text.MediaWiki.Wiktionary.Base (WiktionaryFact)
> import Text.MediaWiki.Wiktionary.English (enParseWiktionary)
> import Text.MediaWiki.Wiktionary.French (frParseWiktionary)
> import Text.MediaWiki.Wiktionary.German (deParseWiktionary)
> import Data.Aeson (encode)

Language handling
=================

The functions for handling different language Wiktionaries are defined in
separate modules. We take an argument that tells us which language the entries
will be in, so we can delegate to the appropriate handler.

> languageHandler :: Language -> Text -> Text -> [WiktionaryFact]
> languageHandler "en"  = enParseWiktionary
> languageHandler "fr"  = frParseWiktionary
> languageHandler "de"  = deParseWiktionary
> languageHandler other = error ("unknown language: " <> (cs (fromLanguage other)))

Top level
=========

> handlePage :: Language -> WikiPage -> IO ()
> handlePage language page = do
>   when (pageNamespace page == "0" && pageRedirect page == Nothing)
>     (mapM_ (println . encode)
>            (languageHandler language (pageTitle page) (pageText page)))

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     (language:_) -> processMediaWikiStdin (handlePage (toLanguage language))
>     _            -> error "Please give a language code as a command-line argument"

