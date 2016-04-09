> {-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
> module Data.LanguageType (Language, fromLanguage, toLanguage) where
> import ClassyPrelude
> import Data.Aeson (ToJSON, toJSON)

Define a Language type that we can keep separate from other strings. This
makes our type signatures clearer, and it's implemented with `newtype`,
so it should have no run-time cost.

> newtype Language = Language Text deriving (Show, Eq, Ord)
> fromLanguage :: Language -> Text
> fromLanguage (Language code) = code
>
> toLanguage :: Text -> Language
> toLanguage code = Language code

A Language can be given with a string literal.

> instance IsString Language where
>   fromString = toLanguage . pack

A Language looks just like a string when you encode it to JSON.

> instance ToJSON Language where
>   toJSON = toJSON . fromLanguage
