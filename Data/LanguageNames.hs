{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Data.LanguageNames where
import WikiPrelude
import Data.LanguageNamesData (languageData)

lookupLanguage :: Language -> Text -> Language
lookupLanguage "en" "Rapanui" = "rap"
lookupLanguage "en" "Tok Pisin" = "tpi"
lookupLanguage "en" "Bokmål" = "nb"
lookupLanguage "de" "International" = "mul"
-- French Wiktionary style does not use complete language names, but a
-- few of them slip in
lookupLanguage "fr" "Français" = "fr"
lookupLanguage "fr" "Anglais" = "en"
lookupLanguage "fr" "Italian" = "it"
lookupLanguage "fr" "Italien" = "it"
lookupLanguage "fr" "Allemand" = "de"
lookupLanguage "fr" "Espagnol" = "es"
lookupLanguage "fr" "conv" = "mul"
lookupLanguage "fr" code = toLanguage code
lookupLanguage lang name = findWithDefault "und" (lang, name) languageMap

entryTuple :: Text -> ((Language, Text), Language)
entryTuple line =
  let entry = splitSeq "," line
      Just lang = index entry 0
      Just name = index entry 1
      Just code = index entry 2
  in ((toLanguage lang, name), toLanguage code)

languageMap :: Map (Language, Text) Language
languageMap = mapFromList (map entryTuple (lines languageDataText))

languageDataText :: Text
languageDataText = pack languageData
