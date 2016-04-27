Compiling and running the code
==============================

This code is meant to be built with [Haskell Stack][stack]. Install Stack and
build this code with:

```sh
stack build
```

[stack]: https://github.com/commercialhaskell/stack

The top-level programs are `wiki2text` and `wiktionary-parser`. `wiki2text`
extracts paragraphs of plain text from a MediaWiki dump in XML format
(ideally Wikipedia). It can be run with:

```sh
bunzip2 -c wikipedia.xml.bz2 | stack exec wiki2text
```

`wiktionary-parser` parses the structure of some
languages' Wiktionaries to extract a stream of lexical facts in JSON format.
It takes one argument, the language code of the Wiktionary it will be parsing.
Each language has its own structure of Wiktionary entries, and so far we
can parse:

- English (`en`)
- French (`fr`)
- German (`de`)

To run it in English, for example:

```sh
bunzip2 -c enwiktionary.xml.bz2 | stack exec wiktionary-parser en
```


Why this code is in Haskell
===========================

I've been facing the problem of how to deal with Wikitext well for a long time.
I didn't originally expect to solve it using Haskell.

The thing about Haskell is that it's designed by mathematicians, and for the
most part, it's also documented for mathematicians. Everything about the language
encourages you to write code that's not about down-to-earth things like functions,
strings, and lists, but instead is about functors, monoids, and monads. This gives
Haskell code a reputation for being incomprehensible to most people.

Now, sometimes a problem comes along that mathematicians, with their lofty
abstractions, are actually much better equipped to solve than a typical
software developer. One of those problems is parsing. Other languages struggle
with parsing while Haskell just *nails* it.

I am not much of a mathematician. I like functional programming, but I also
like writing straightforward understandable Python code.  But I needed to be
able to write a powerful, extensible parser for Wikitext, and I could tell my
Python code wasn't going to cut it. I looked at my available options for this
kind of parsing, and found that they amounted to:

- Something based on Parsec in Haskell
- Something based on Parsec but in another programming language, imperfectly
  pretending to be Haskell
- Awful spaghetti hacks

There are already Wikitext parsers that are awful spaghetti hacks, and I can't
build on those. (The reference implementation -- MediaWiki itself -- would be a
great example, but so are the various Java-based parsers I've seen.) So the
next best choice is Haskell.

And if nobody else, another audience I'm writing for is my future self. I can
imagine a year from now, coming back to this code, saying "what the hell was I
thinking with all these monads", and wanting to start over, unless I write some
documentation that explains what I was thinking.

This looks like Markdown, where's the code?
-------------------------------------------

One thing I love about Haskell is the Literate Haskell (`.lhs`) format. The
Haskell compiler can interpret it without any pre-processing, and it encourages
documentation as the rule and code as the exception.

Lines that start with the character `>` are code. There won't be any of that
until I get to the header. The rest is Markdown. The documentation tool
`pandoc` can convert this all into nicely-formatted HTML, but just reading the
Markdown + Haskell source should do the job too.

You could start reading with `WikiPrelude.lhs`, which sets up the functions
we want to be available in all our modules, or `Text/Wiki/MediaWiki.lhs`,
which performs the basic level of parsing for MediaWiki syntax.


