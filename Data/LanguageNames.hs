{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Data.LanguageNames where
import WikiPrelude
import Data.LanguageNamesData (languageData)

unknownCode :: Text -> Text
unknownCode name =
  concat [
    "und-x-",
    intercalate "-" (splitSeq " " name)]

lookupLanguage :: Language -> Text -> Text
lookupLanguage "fr" "conv" = "mul"
lookupLanguage "fr" code = code
lookupLanguage lang name = findWithDefault (unknownCode name) (lang, name) languageMap

entryTuple :: Text -> ((Language, Text), Text)
entryTuple line =
  let entry = splitSeq "," line
      Just lang = index entry 0
      Just name = index entry 1
      Just code = index entry 2
  in ((toLanguage lang, name), code)

languageMap :: Map (Language, Text) Text
languageMap = mapFromList (map entryTuple (lines languageDataText))

languageDataText :: Text
languageDataText = pack languageData
