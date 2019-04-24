Wikiparsec: tools that parse Wikipedia and Wiktionary
=====================================================

Author: Robyn Speer

Wikiparsec provides tools for parsing the complex MediaWiki
syntax that appears on Wikipedia and Wiktionary, for the purpose of information
extraction.

I'm aware that many other tools do the same, but I think most of them are too
sloppy about their parsing. Wikiparsec is designed for cases where it's
important to parse the page as correctly as possible (without running an embedded
instance of MediaWiki), and extract detailed information from that parse.

We're not outputting a fully-general tree structure of everything going on in
the Wikitext (if you output that, you *still* have a parsing problem). Instead,
the parser is connected to functions that extract relevant information for the
problem at hand, and those functions are responsible for the output.

Wikiparsec is used to provide dictionary definitions to
[ConceptNet](http://conceptnet.io).


Compiling and running the code
------------------------------

This code is meant to be built with [Haskell Stack][stack]. Install Stack and
dependencies:

```
sudo apt install stack libicu-dev
stack upgrade
```

If Stack upgraded itself, you need to start a new shell, or else the old path
to Stack will be cached.

Then build this code with:

```sh
stack clean
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
---------------------------

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

This looks like Markdown, where's the code?
-------------------------------------------

One thing I love about Haskell is the Literate Haskell (`.lhs`) format. The
Haskell compiler can interpret it without any pre-processing, and it encourages
documentation as the rule and code as the exception.

Lines that start with the character `>` are code. The rest is Markdown. The
documentation tool `pandoc` can convert this all into nicely-formatted HTML,
which you might even be reading right now, but just reading the Markdown +
Haskell source should do the job too.

You could start reading with `WikiPrelude.lhs`, which sets up the functions
we want to be available in all our modules, or `Text/Wiki/MediaWiki.lhs`,
which performs the basic level of parsing for MediaWiki syntax.

Ideally, this documentation is written with a target audience of people who
are at least somewhat familiar with functional programming, but don't
necessarily know a lot of details about Haskell.

One member of that audience is myself. I can imagine a year from now, coming
back to this code, saying "what the hell was I thinking with all these monads",
and wanting to start over, unless I write some documentation that explains what
I was thinking, in terms of both how the code is designed and how Haskell works
in general.

