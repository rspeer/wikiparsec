> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

`Text.MediaWiki.XML`: extracting Wiki pages from XML dumps
==========================================================

Parsing MediaWiki involves a stack of syntaxes nested inside each other:

1. XML
2. HTML tags, entities, and comments
3. The section structure of a Wiki page
4. Wiki formatting, such as `[[links]]` and `{{templates}}`

The purpose of this module is to extract the page contents from a MediaWiki XML
dump, and send the resulting contents on to `Text.MediaWiki.HTML`, the next
step of parsing.

As a reference, this is what a page of a MediaWiki XML dump looks like:

```xml
  <page>
    <title>Albedo</title>
    <ns>0</ns>
    <id>39</id>
    <revision>
      [... lots of metadata tags ...]
      <text xml:space="preserve">{{Other uses}}
        {{Use dmy dates|date=June 2013}}
        [[File:Albedo-e hg.svg|thumb|Percentage of diffusely reflected sunlight in relation to various surface conditions]]

        '''Albedo''' ({{IPAc-en|æ|l|ˈ|b|iː|d|oʊ}}) is the &quot;whiteness&quot; of a surface. It is a '''reflection coefficient''', and has a value of less than one.
        [... rest of article text ...]
      </text>
    </revision>
  </page>
```

The elements we care about within a `<page>` are `<ns>` (the *namespace*),
`<title>`, `<text>`, and `<redirect>` if it exists.

> module Text.MediaWiki.XML where
> import WikiPrelude

We use Expat SAX for streaming XML decoding:

> import qualified Text.XML.Expat.SAX as SAX

The output will be sent onward to the HTML parser:

> import Text.MediaWiki.HTML (extractWikiTextFromHTML)

Data structures
---------------

When we extract sub-tags of the page, we'll represent them as an association
list from ByteStrings to ByteStrings. (An association list, which is just a
list of pairs, will work fine as a mapping here because the mapping is small:
it will only ever contain 4 elements.)

> type ByteStringAssoc = [(ByteString, ByteString)]

`lookup key aList` is a standard function that finds the item in an association
list whose first part is `key`, and returns `Just` the item's second part (its
value).

Most of the elements we're looking for are present on every article in the XML
dump, so we define `justLookup` here, which gets the unwrapped value. It
unwraps `Just x` into `x` by using `fromMaybe` with an error as the default
case.

> justLookup :: ByteString -> ByteStringAssoc -> ByteString
> justLookup key aList = fromMaybe (error ("Missing tag: " ++ (show key))) (lookup key aList)

Once we've found the subtags, then we'll wrap them up as a record called
`WikiPage`, making sure to decode the values appropriately.

> data WikiPage = WikiPage {
>   pageNamespace :: Text,
>   pageTitle :: Text,
>   pageText :: Text,
>   pageRedirect :: Maybe Text
> } deriving (Show, Eq)

There's one tag that is not always present, which is `redirect`, so we don't
use `justLookup` on that tag. Instead, we get its value as a `Maybe ByteString`,
then decode it into a `Maybe Text`.

Did you know that `Maybe` is a monad? The "sequence of things you can do" is
perhaps not as obvious as it is with IO, but the idea is that you can string
together operations that take unwrapped values and produce `Maybe` values, and
if any of them ever produces `Nothing`, the result of the whole chain of computation
is `Nothing`.

Something we can do with monads besides sequence things together is to apply
a normal-looking function to the value that's wrapped by the monad. That's what
the "lift" operator does, which is typically spelled `<$>` (even in other
functional programming languages). We'll use it here to `decodeUtf8` the value
inside the `Maybe`, or if the value is `Nothing`, leave it that way.

> makeWikiPage :: ByteStringAssoc -> WikiPage
> makeWikiPage subtags = WikiPage {
>    pageNamespace = decodeUtf8 (justLookup "ns" subtags),
>    pageTitle = decodeUtf8 (justLookup "title" subtags),
>    pageText = extractWikiTextFromHTML (justLookup "text" subtags),
>    pageRedirect = decodeUtf8 <$> lookup "redirect" subtags
> }

To restate what the above does: `makeWikiPage` takes in the association list of
XML tags we found. It runs the page text through `extractWikiTextFromHTML` and
decodes the other values as UTF-8 plain text.  It returns all these decoded
values in a `WikiPage` record.

SAX events
----------

SAX, the Streaming API for XML, is a form of XML parser that isn't given a
tree of tags, just a sequence of events such as the beginnings and ends
of tags. Using a SAX parser requires implementing handlers for each of these
events, keeping track of whatever state is necessary.

To find the appropriate tags in Wikipedia's XML dump, we're going to run such
a state machine, which reacts to SAX events and turns them into WikiPages.

> findPageTags = handleEventStream [] []

The state of `handleEventStream` is contained in its first two arguments. The
first is a ByteStringAssoc of the relevant tags we've found for the current
article, and the second is the text of the current tag, which we also have to
build up statefully because it could arrive in chunks instead of all at once.

So the arguments are three lazy lists:

1. Tags we haven't put into WikiPages yet (`subtags`)
2. Text we haven't put into tags yet (`chunks`)
3. SAX events we have left to handle

and the output is a lazy list of WikiPages we get when we handle those events.

This is a good example of what lazy lists are for, because it means we can
start getting results and passing them on to another function before we're done
parsing all of the XML. (I would also gladly use imperative code for this, but
that's not a good option here in Haskell-land.)

> handleEventStream :: ByteStringAssoc -> [ByteString] -> [SAX.SAXEvent ByteString ByteString] -> [WikiPage]

If there are no SAX events, there are no WikiPages. This is the base case we
encounter at the end of the stream.

> handleEventStream subtags chunks [] = []

If the next event is a StartElement for the `<page>` tag, clear the
accumulated tags and text (there might be some that came from miscellaneous elements
before the first page), and continue parsing.

> handleEventStream subtags chunks ((SAX.StartElement "page" attrs):rest) = handleEventStream [] [] rest

If the next event is a `<redirect>` tag, extract the title from its
attributes, and add `("redirect", title)` to the list of pending tags.

> handleEventStream subtags chunks ((SAX.StartElement "redirect" attrs):rest) =
>   let title = justLookup "title" attrs
>   in handleEventStream (("redirect",title):subtags) [] rest

If the next event is some other opening tag, clear the pending text.

> handleEventStream subtags chunks ((SAX.StartElement elt attrs):rest) = handleEventStream subtags [] rest

If the next event is the EndElement representing `</page>`, pass our
accumulated subtags to `makeWikiPage`. The resulting WikiPage will be the next
result in our output list.  Results after that, of course, are whatever we find
when we handle the rest of the events. We clear out the `subtags` and `chunks`
we got for this page, because we're done with them now.

> handleEventStream subtags chunks ((SAX.EndElement "page"):rest) = ((makeWikiPage subtags):(handleEventStream [] [] rest))

If the next event is CharacterData, we take the chunk of bytes it contains and
cons it onto our `chunks` list (that means inserting it at the front of a
linked list).

> handleEventStream subtags chunks ((SAX.CharacterData t):rest) = handleEventStream subtags (t:chunks) rest

If the next event is an EndElement but not the end of a page, we put the
element name we saw and its text into our list of subtags. The text arrived in
chunks that we consed together into a linked list, so they're backwards. We
reverse the order of the list, and then using the fact that ByteString is a
Monoid, we `mconcat` them into a single ByteString.

> handleEventStream subtags chunks ((SAX.EndElement elt):rest) = handleEventStream ((elt, mconcat (reverse chunks)):subtags) [] rest

If the next event is an XML parse error, well, raise an error.

> handleEventStream subtags chunks ((SAX.FailDocument (SAX.XMLParseError err loc)):rest) =
>   error ("XML parse error: " ++ err ++ " at " ++ (show loc))

That should be all the cases. If an XML element we weren't expecting appears,
raise an error.

> handleEventStream subtags chunks (misc:rest) = error ("Can't handle element: " ++ show misc)


The whole XML-parsing process
-----------------------------

We have a stream of XML data coming from somewhere.  We scan through it,
extracting `WikiPage` records. We pass those records on to a `sink` that says
what to do with them, which is not decided by this module.

`processMediaWikiContent` is the key function here. Its input is a lazy
ByteString (`LByteString`), which means that we can get bytes from the front of
it without having to read the whole thing first.

The result of `SAX.parse` is a lazy list of `SAXEvents`, which we scan through
and turn into `WikiPage` records using the SAX handlers defined above,
particularly `findPageTags`.

The `sink` is a function that takes our `WikiPage` records and does something
with them that will go to stdout (whose type is `IO ()`). The result of this
whole function is that a bunch of things happen to stdout, so its return type
is also `IO ()`.

We're not defining a `sink` here. The sink is different depending on what data
you're working with and what you're intending to do with it, and that's for the
top-level program to decide.

> processMediaWikiContent :: LByteString -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiContent content sink = do
>   let events = SAX.parse SAX.defaultParseOptions content
>   mapM_ sink (findPageTags events)

Finally, we define where this XML stream is coming from. It's coming from an
open file handle, specifically stdin, because the data is being piped into this
program straight out of `bunzip2`.

> processMediaWikiStream :: Handle -> (WikiPage -> IO ()) -> IO ()
> processMediaWikiStream source sink = do
>   content <- hGetContentsLazy source
>   processMediaWikiContent content sink
>
> processMediaWikiStdin = processMediaWikiStream stdin
