`Data.LanguageType`: gives language codes their own type
========================================================

This module defines a Language type that we can keep separate from other strings. A
Language contains nothing but a Text of its language code, such as "en",
but it's a different type.

This makes our type signatures clearer, and it's implemented with `newtype`, so
it should have no run-time cost.

> {-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
> module Data.LanguageType (Language, fromLanguage, toLanguage) where
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
