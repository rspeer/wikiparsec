> {-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ConstraintKinds #-}

The WikiPrelude is a small extension of the ClassyPrelude, designed to
include some more types and functions that we'll need throughout the parser.

Here's what we're exporting from the module:

> module WikiPrelude (
>   module ClassyPrelude,
>   module Data.String.Conversions,
>   module Data.LanguageType,
>   Joinable,
>   replace,
>   get, getPrioritized
>   ) where

And now we either import or define all those things.

> import ClassyPrelude
> import Data.String.Conversions hiding ((<>))
> import Data.LanguageType
> import qualified Data.Text as T
>
> replace = T.replace

While it seems popular in Haskell to baffle the uninitiated with
category-theory terminology, I would rather make the code a little more
accessible.

There's a pretty useful type-class called `Monoid`. Mathematically, a monoid is
something that you can do an associative operation to (like adding or
concatenating), and the operation has an identity (like 0 or []).

Practically, when we use `Monoid`, it's to say "this thing is a sequence of
some kind, like a string or a list, and we know how to concatenate it". So
let's alias `Monoid` to `Joinable`. ("Concatenatable" is a bit too hard to
spell.)

> type Joinable a = Monoid a

In many situations we have a mapping whose values are Joinables. This lets us
write the convenient `get` function, which looks up a key in the mapping, or
returns an empty value if it's not there.

If the values of the map are strings or Texts, the empty value you get is "".
If the values are lists, the empty value is [].

> get :: (IsMap map, Joinable (MapValue map)) => ContainerKey map -> map -> MapValue map
> get = findWithDefault mempty

`getPrioritized` is like `get`, but tries looking up multiple different keys
in priority order. It returns the empty value only if it finds none of them.

> getPrioritized :: (IsMap map, Joinable (MapValue map)) => [ContainerKey map] -> map -> MapValue map
> getPrioritized (key:rest) map = findWithDefault (getPrioritized rest map) key map
> getPrioritized [] map         = mempty
