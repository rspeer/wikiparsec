`WikiPrelude` - A custom Prelude for Wiki parsing
=================================================

The Haskell standard library is called the 'Prelude'. There is general
agreement that the Prelude is not very good code and should be replaced with
something else.  There is absolutely no agreement on what it should be replaced
with.

I built this code on an alternative Haskell standard library, the Classy
Prelude, because it's got a good idea of how to deal with different data types
such as Texts and ByteStrings. Instead of prefixed functions that deal with one
data type, ClassyPrelude uses generic functions that can apply to any data type
that does the right things.

Instead of having to use `T.append` to append texts, and `Char8.append` to
append ByteStrings, or whatever, we just use `append` to append things that can
be appended. This leads to cleaner code and easier refactoring.


Monads and monoids, oversimplified
----------------------------------

A [classic joke][] about Haskell defines these terms: "A monad is a monoid in
the category of endofunctors, what's the problem?"

[classic joke]: http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html

It's funny because it's true. Haskell works best when you embrace its
poorly-named mathematical abstractions, and monads and monoids are the ones
that are going to come up in this code. But in actual Haskell code, the full
generality of the mathematical abstraction usually doesn't matter; it's all
about how you use it.

So let's oversimplify what these things are, the way we oversimplify other
mathematical concepts like "matrix" when programming.

If you're a Haskell programmer reading this, you'll find this section to be old
hat. I don't know many Haskell programmers, though. This documentation is for
other people who come across this code and are just familiar with functional
programming in general.

Monoids are things you can concatenate
--------------------------------------

A **monoid** is a type of thing that can be empty and can be concatenated.
Some monoids you'll encounter in this code are Unicode text, ByteStrings, and
lists. Sets also work, if you think about the "union" operation as being like
concatenation.

(A mathematician might say I'm overlooking some monoids that are a big deal,
like addition of integers. But in Haskell code, you wouldn't *use* a monoid to
add integers. You'd use good old `+` for that. Monoids are for things you need
to concatenate.)

When I'm willing to call all these sequencey things Monoids, then instead of
having to use awkwardly-namespaced functions for dealing with all these types
separately (like `T.append` for text, versus `BS.append` for bytestrings), I
can use `mappend` to append whatever monoidy things I have, and `mempty` to
get an empty one.

By the way, Haskell programmers show their apprecation for functions they find
really important by giving them infix operators. So `mappend list1 list2` is
also spelled `list1 <> list2`.

Monads are stateful things you can do
-------------------------------------

A **monad** is a way to do stateful things in sequence. The advantage of using
a monad is that it keeps track of the state for you while you just return a
result. Without monads, you might have to write functions that take in
`(actualInput, state)` and return `(actualOutput, newState)`, which would be
repetitive and error-prone.

In Haskell, doing any sort of I/O requires an IO monad: your code is changing
the state of what it's read from and written to the rest of the system.

Parsing is a monad. Your state is where you are in the input. When you parse
something and move the cursor forward through the input, that modifies the
state.

Monads are important enough to Haskell that they get their own syntax, the `do`
block, which just lets you list a bunch of state-changing things you need to
do to a monad, in order.

Because IO and parsing are the same kind of thing, they look similar in the
type system. A function of type `IO Text` is a function that does some IO and
then returns some Text. A function of type `Parser Text` is a function that
parses some input and then returns some Text.


Here's where the actual code starts
-----------------------------------

> {-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

The WikiPrelude is a small extension of the ClassyPrelude, designed to
include some more types and functions that we'll need throughout the parser.

Here's what we're exporting from the module:

> module WikiPrelude (
>   module ClassyPrelude,
>   module Data.String.Conversions,
>   module Data.LanguageType,
>   module Control.Monad.Writer,
>   replace, splitOn, stripSpaces, dropAround, dropWhileEnd, toTitle,
>   breakOn, breakOnEnd, listTakeWhile, listDropWhile,
>   get, getAll, getPrioritized, put, nonEmpty,
>   println
>   ) where

Some of these exports are just re-exporting things we can import:

> import ClassyPrelude hiding (takeWhile)
> import qualified ClassyPrelude as P
> import Data.String.Conversions hiding ((<>))
> import Data.LanguageType
> import Control.Monad.Writer (Writer, writer, pass, runWriter, execWriter)
> import qualified Data.Text as T

Text operations
---------------

These are functions that apply to Text that for some reason didn't make it into
the ClassyPrelude.

> replace = T.replace
> splitOn = T.splitOn
> breakOn = T.breakOn
> breakOnEnd = T.breakOnEnd
> dropAround = T.dropAround
> dropWhileEnd = T.dropWhileEnd
> toTitle = T.toTitle

Another kind of standard thing we need to do is trim spaces from the start and
end of a string:

> stripSpaces :: Text -> Text
> stripSpaces = dropAround (\c -> c == ' ' || c == '\n')

Writing any sort of text to stdout:

> println :: (IOData a) => a -> IO ()
> println = hPutStrLn stdout


List operations
---------------

The name `takeWhile` has conflicting definitions in ClassyPrelude and
Attoparsec, so we need to rename the ClassyPrelude one.

> listTakeWhile :: (a -> Bool) -> [a] -> [a]
> listTakeWhile = P.takeWhile
>
> listDropWhile :: (a -> Bool) -> [a] -> [a]
> listDropWhile = P.dropWhile


Mapping operations
------------------

In many situations we have a mapping whose values are sequences. This lets us
write the convenient `get` function, which looks up a key in the mapping, or
returns an empty sequence if it's not there.

What I'm calling a sequence is what Haskell calls a monoid -- see the section
"Monoids are things you can concatenate" above.

> get :: (IsMap map, Monoid (MapValue map)) => ContainerKey map -> map -> MapValue map
> get = findWithDefault mempty

`getPrioritized` is like `get`, but tries looking up multiple different keys
in priority order. It returns the empty value only if it finds none of them.

> getPrioritized :: (IsMap map, Monoid (MapValue map)) => [ContainerKey map] -> map -> MapValue map
> getPrioritized (key:rest) map = findWithDefault (getPrioritized rest map) key map
> getPrioritized [] map         = mempty

`getAll` is a step in `getPrioritized`. It takes a list of keys and a mapping,
and returns the list of values of those keys that exist.

> getAll :: (IsMap m, Monoid (MapValue m)) => [ContainerKey m] -> m -> [MapValue m]
> getAll keys m = catMaybes (map (\key -> lookup key m) keys)

Undoing our default empty sequence by turning empty sequences into Nothing:

> nonEmpty :: (Monoid a, Eq a) => Maybe a -> Maybe a
> nonEmpty val =
>   case val of
>     Just something -> if (something == mempty) then Nothing else val
>     Nothing -> Nothing

I found myself having to build up a map with pieces from another map often
enough that the syntax was getting quite repetitive. So this `put` function
turns the process of building a map into a Writer monad, allowing us to use
`do` syntax and create a domain-specific language for building mappings.
This syntax will be extended in `Text.MediaWiki.Wiktionary.Base`.

> put :: (IsMap map, Monoid (MapValue map), Eq (MapValue map)) => ContainerKey map -> MapValue map -> Writer map (MapValue map)
> put key value =
>   if (value == mempty)
>     then writer (value, mempty)
>     else writer (value, singletonMap key value)

