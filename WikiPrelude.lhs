> {-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ConstraintKinds #-}

The WikiPrelude is a small extension of the ClassyPrelude, designed to
include some more types and functions that we'll need throughout the parser.

Here's what we're exporting from the module:

> module WikiPrelude (
>   module ClassyPrelude,
>   module Data.String.Conversions,
>   module Data.LanguageType,
>   replace, splitOn,
>   get, getPrioritized
>   ) where

And now we either import or define all those things.

> import ClassyPrelude
> import Data.String.Conversions hiding ((<>))
> import Data.LanguageType
> import qualified Data.Text as T
>
> replace = T.replace
> splitOn = T.splitOn

In many situations we have a mapping whose values are sequences. This lets us
write the convenient `get` function, which looks up a key in the mapping, or
returns an empty sequence if it's not there.

If the values of the map are strings or Texts, the empty value you get is "".
If the values are lists, the empty value is [].

> get :: (IsMap map, Monoid (MapValue map)) => ContainerKey map -> map -> MapValue map
> get = findWithDefault mempty

`getPrioritized` is like `get`, but tries looking up multiple different keys
in priority order. It returns the empty value only if it finds none of them.

> getPrioritized :: (IsMap map, Monoid (MapValue map)) => [ContainerKey map] -> map -> MapValue map
> getPrioritized (key:rest) map = findWithDefault (getPrioritized rest map) key map
> getPrioritized [] map         = mempty
