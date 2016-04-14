{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Data.LanguageNames where
import WikiPrelude
import Data.LanguageNamesData (languageData)

unknownCode :: Text -> Language
unknownCode name = toLanguage $
  concat [
    "und-x-",
    intercalate "-" (splitSeq " " name)]

lookupLanguage :: Language -> Text -> Language
lookupLanguage "fr" "conv" = "mul"
lookupLanguage "fr" code = toLanguage code
lookupLanguage lang name = findWithDefault (unknownCode name) (lang, name) languageMap

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
