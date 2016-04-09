> {-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

The WikiPrelude is a small extension of the ClassyPrelude, designed to
include some more types and functions that we'll need throughout the parser.

Here's what we're exporting from the module:

> module WikiPrelude (
>   module ClassyPrelude,
>   module Data.LanguageType,
>   replace,
>   get, getPrioritized
>   ) where

And now we either import or define all those things.

> import ClassyPrelude
> import Data.LanguageType
> import qualified Data.Text as T
> 
> replace = T.replace

Get a value from a mapping. If it isn't in the mapping, return an empty
value.

If the values of the map are strings or Texts, the empty value you get is "".
If the values are lists, the empty value is []. In general, your values are
sequences, and the default is the empty sequence.

This type declaration involves using the term "Monoid", and we're using it to
mean a sequence of things that we don't particularly need to know what they
are. When reading Haskell code, you can think "sequence" whenever you read
"Monoid" and you will rarely be led astray.

> get :: (IsMap map, Monoid (MapValue map)) => ContainerKey map -> map -> MapValue map
> get = findWithDefault mempty

`getPrioritized` is like `get`, but tries looking up multiple different keys
in priority order. It returns the empty value only if it finds none of them.

> getPrioritized :: (IsMap map, Monoid (MapValue map)) => [ContainerKey map] -> map -> MapValue map
> getPrioritized (key:rest) map = findWithDefault (getPrioritized rest map) key map
> getPrioritized [] map         = mempty
