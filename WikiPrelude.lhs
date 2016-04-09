> {-# LANGUAGE NoImplicitPrelude #-}

The WikiPrelude is a small extension of the ClassyPrelude, designed to
include some more types and functions that we'll need throughout the parser.
Mostly it just imports modules and re-exports them.

> module WikiPrelude (
>   module ClassyPrelude,
>   module Data.LanguageType,
>   replace
>   ) where
> import ClassyPrelude
> import Data.LanguageType
> import qualified Data.Text as T
> 
> replace = T.replace
