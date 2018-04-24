`Data.LanguageType`: gives language codes their own type
========================================================

This module defines a Language type that we can keep separate from other strings. A
Language contains nothing but a Text of its language code, such as "en",
but it's a different type.

This makes our type signatures clearer, and it's implemented with `newtype`, so
it should have no run-time cost.

> {-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
> module Data.LanguageType (Language, fromLanguage, toLanguage, fixLanguageCode) where
> import qualified Data.Text as T
> import ClassyPrelude
> import Data.Aeson (ToJSON, toJSON)

We define Language as a type containing nothing but a Text. A Language can be
displayed to the console and compared to other Languages in the obvious ways
that we can just ask Haskell to derive.

> newtype Language = Language Text deriving (Show, Eq, Ord)

`fromLanguage` and `toLanguage` convert between Languages and Texts.

> fromLanguage :: Language -> Text
> fromLanguage (Language code) = code
>
> toLanguage :: Text -> Language
> toLanguage code = Language code

A string literal can represent a Language when type inference requires it.

> instance IsString Language where
>   fromString = toLanguage . pack

A Language looks just like a string when you encode it to JSON.

> instance ToJSON Language where
>   toJSON = toJSON . fromLanguage

In etymology sections, we need to recognize abbreviations for languages that
are not actual language codes, sometimes because they stand for etymological
descriptions such as "Late Latin". `fixLanguageCode` converts common instances
of these into similar real language codes.

Common examples of these codes, such as "BE." for "British English", are
also sometimes written without the period at the end, so we strip final
periods.

> fixLanguageCode :: Text -> Text
> fixLanguageCode text
>   | isSuffixOf "." text = mapBadCodes (T.dropEnd 1 text)
>   | otherwise = mapBadCodes text

> mapBadCodes :: Text -> Text
> mapBadCodes "AE" = "en-US"
> mapBadCodes "AG" = "de-AT"
> mapBadCodes "BE" = "en-GB"
> mapBadCodes "CF" = "fr-CA"
> mapBadCodes "Ecclesiastical Latin" = "la"
> mapBadCodes "EL" = "la"
> mapBadCodes "Ins.Sc" = "sco"
> mapBadCodes "Koine" = "grc-koi"
> mapBadCodes "Late Latin" = "la"
> mapBadCodes "LL" = "la"
> mapBadCodes "Lunfardo" = "es-lun"
> mapBadCodes "Medieval Latin" = "la"
> mapBadCodes "Mid.Sc" = "sco"
> mapBadCodes "MIr" = "ira-mid"
> mapBadCodes "ML" = "la"
> mapBadCodes "NL" = "la"
> mapBadCodes "Nor.Sc" = "sco"
> mapBadCodes "OIr" = "ira-old"
> mapBadCodes "ONF" = "fro"
> mapBadCodes "O.Sc" = "sco"
> mapBadCodes "pinhua" = "yue"
> mapBadCodes "pregrc" = "qfa-sub-grc"
> mapBadCodes "RL" = "la"
> mapBadCodes "Sha" = "wuu-sha"
> mapBadCodes "Sou.Sc" = "sco"
> mapBadCodes "Tax" = "mul"
> mapBadCodes "Uls.Sc" = "sco"
> mapBadCodes "VG" = "de-AT"
> mapBadCodes "VL" = "la"
> mapBadCodes other = other
