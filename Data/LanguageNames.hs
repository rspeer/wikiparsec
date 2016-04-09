{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Data.LanguageNames where
import ClassyPrelude
import Data.LanguageNamesData (languageData)

unknownCode :: Text -> Text
unknownCode name =
  concat [
    "und-x-",
    intercalate "-" (splitSeq " " name)]

lookupLanguage :: ByteString -> ByteString -> ByteString
lookupLanguage lang name = encodeUtf8 $ lookupLanguageT (decodeUtf8 lang) (decodeUtf8 name)

lookupLanguageT :: Text -> Text -> Text
lookupLanguageT "fr" "conv" = "mul"
lookupLanguageT "fr" code = code
lookupLanguageT lang name = findWithDefault (unknownCode name) (lang, name) languageMap

entryTuple :: Text -> ((Text, Text), Text)
entryTuple line =
  let entry = splitSeq "," line
      Just lang = index entry 0
      Just name = index entry 1
      Just code = index entry 2
  in ((lang, name), code)

languageMap :: Map (Text, Text) Text
languageMap = mapFromList (map entryTuple (lines languageDataText))

languageDataText :: Text
languageDataText = pack languageData
