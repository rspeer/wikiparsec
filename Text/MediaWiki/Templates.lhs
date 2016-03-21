> {-# LANGUAGE OverloadedStrings #-}

Handling templates
==================

> module Text.MediaWiki.Templates where
> import qualified Text.MediaWiki.AnnotatedString as A
> import Text.MediaWiki.AnnotatedString (AnnotatedString, Annotation)
> import Text.MediaWiki.AList (getDefault)
> import Data.ByteString (ByteString)

Parsing templates the same way they're parsed on Wikipedia or Wiktionary would
be an insanely complicated and time-consuming process, as their actions are
interpreted from an ad-hoc programming language *written in Wikitext* that
itself has to be parsed. On top of that, some of them run PHP or Lua code via
extensions.

We assume here that we don't want to be able to fill in every template; we just
want to output something reasonable from the most common templates, and in most
cases output nothing at all.

First: the syntax of a template is represented as an association list from
parameter names to values.  Both the names and the values are ByteStrings.
Conveniently, this makes it the same type as an Annotation (from
AnnotatedString).

To customize the values of templates for different wikis, we'll be passing
around an object called a TemplateProc, which looks up the name of the template
and returns a way to manipulate the text. That is:

- It takes in a ByteString, the same one that's in the "0" slot of the
  template. (We pass this as a separate argument to make dispatch much
  easier, because any non-trivial computation you do will depend on the name of
  the template.)

- It returns a function that takes an Annotation of the template's arguments
  and returns an AnnotatedString, which we're calling a TemplateAction.

> type TemplateProc = ByteString -> TemplateAction
> type TemplateAction = (Annotation -> AnnotatedString)
>
> noTemplates :: TemplateProc
> noTemplates = const skipTemplate

Two useful template actions. `skipTemplate` outputs the empty string no matter
what the arguments of the template are. `idTemplate` returns the name of the
template. `useArg` returns a given named or positional argument.

Keep in mind that template arguments are always strings, even the positional
ones such as "1". We do this to keep types consistent as we emulate PHP.

> skipTemplate :: TemplateAction
> skipTemplate = const ""
>
> useArg :: ByteString -> TemplateAction
> useArg arg = A.fromBytes . (getDefault "" arg)
> idTemplate = useArg "0"
>
> evalTemplate :: TemplateProc -> Annotation -> AnnotatedString
> evalTemplate tproc tdata =
>   let action = tproc (getDefault "" "0" tdata) in action tdata
