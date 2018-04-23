`Text.MediaWiki.Wiktionary.Base`: parsing Wiktionary in general
===============================================================

This file defines how to parse Wiktionary entries, as a layer above the basic
handling of wiki syntax in `Text.MediaWiki.WikiText`.

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, FlexibleContexts #-}
> module Text.MediaWiki.Wiktionary.Base where
> import WikiPrelude
> import Text.MediaWiki.WikiText
> import Text.MediaWiki.ParseTools
> import Text.SplitUtils
> import Text.MediaWiki.AnnotatedText
> import Text.MediaWiki.Templates
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator
> import Data.Aeson (ToJSON, toJSON, (.=), encode, object)
> import Data.LanguageNames
> import Data.LanguageType
> import Text.Language.Normalize (normalizeText)
> import Text.MediaWiki.HTML (extractWikiTextFromHTML)
> import Text.Show.Unicode

The `WiktionaryTerm` data type
------------------------------

A WiktionaryTerm is a piece of text that can be defined on Wiktionary. It is
defined by its term text, which is required, along with other fields which may
be unknown or missing: the language it's in, a label for its word sense, its
part of speech, and its etymology label (which is like a much broader word
sense).

(Wiktionary entries for etymologically-unrelated homographs separate them into
sections named, for example, "Etymology 1" and "Etymology 2". In these cases,
"1" and "2" are the etymology labels. Most words only have a single etymology,
and by default their etymology label is "1".)

> data WiktionaryTerm = WiktionaryTerm {
>   wtText :: Text,
>   wtLanguage :: Maybe Language,
>   wtSense :: Maybe Text,
>   wtPos :: Maybe Text,
>   wtEtym :: Maybe Text
> } deriving (Eq)

We export a term to JSON by constructing an object that associates keys with
the record values that are `Just val`, and leaves out the values that are
`Nothing`.

> instance ToJSON WiktionaryTerm where
>   toJSON term =
>     let maybePairs = [("text", Just (wtText term)),
>                       ("language", fromLanguage <$> (wtLanguage term)),
>                       ("pos", wtPos term),
>                       ("etym", wtEtym term),
>                       ("sense", wtSense term)]
>         existingPairs = mapMaybe moveSecondMaybe maybePairs
>     in object [key .= value | (key, value) <- existingPairs]

`mapMaybe` is a version of `map` that throws out `Nothing` values and unwraps
the rest. To use it with our pairs, we need `moveSecondMaybe`, which is pretty
much defined by its type signature.

> moveSecondMaybe :: (a, Maybe b) -> Maybe (a, b)
> moveSecondMaybe (first, Just second) = Just (first, second)
> moveSecondMaybe (first, Nothing)     = Nothing

Now that we have these JSON representations, we can also use them as the
string representation in the REPL. First we convert the term to a JSON value,
then `encode` it to a ByteString.

To convert this to Unicode that Haskell can be convinced to show with
`Text.Show.Unicode`, we coerce it with `cs`, the all-purpose string converter.
We get `cs` from `Data.String.Conversions` in our prelude. It applies the
appropriate string conversion depending on the type constraints, and assumes
UTF-8 everywhere.

> instance Show WiktionaryTerm where
>   show = cs . encode . toJSON

`term` is a constructor for WiktionaryTerms. Because many of the fields of a
WiktionaryTerm are optional, it takes a single argument that is a list of Texts.
This list can contain:

- Element 0: the text of the term
- Element 1: the language code
- Element 2: the part of speech
- Element 3: the etymology label
- Element 4: the sense label

Elements after the end of the list will become `Nothing`. Elements in the middle
can also be set to `Nothing` by giving the empty string as their value. (This is
why we defined `nonEmpty` as part of our monoid toolkit in the WikiPrelude.)

> term :: [Text] -> WiktionaryTerm
> term items =
>   let language = toLanguage (fromMaybe "und" (index items 1)) in
>     WiktionaryTerm {
>       wtText = normalizeText language (fromMaybe (error "term is empty") (index items 0)),
>       wtLanguage = toLanguage <$> (index items 1),
>       wtPos = nonEmpty (index items 2),
>       wtEtym = nonEmpty (index items 3),
>       wtSense = nonEmpty (index items 4)
>       }

Two more constructors for straightforward cases. `simpleTerm` takes in the language code and
the text, and produces a WiktionaryTerm.

> simpleTerm :: Language -> Text -> WiktionaryTerm
> simpleTerm language text = term [text, fromLanguage language]

`termPos` is similar to `simpleTerm`, but takes the part of speech as a third argument.

> termPos :: Language -> Text -> Text -> WiktionaryTerm
> termPos language text pos = term [text, fromLanguage language, pos]


The `WiktionaryFact` data type
------------------------------

A WiktionaryFact expresses a relationship between terms that we can extract
from a page. Much like a ConceptNet edge, it's defined by a relation `rel` that
points `from` one term `to` another term.

> data WiktionaryFact = WiktionaryFact Text WiktionaryTerm WiktionaryTerm deriving (Show, Eq)
>
> instance ToJSON WiktionaryFact where
>   toJSON (WiktionaryFact rel from to) = object ["rel" .= rel, "from" .= from, "to" .= to]

`makeFact` or one of its derived functions should be used to create
WiktionaryFacts.

Sometimes we extract data that goes in the opposite direction form the relation
we want: for example, instead of discovering which word `from` is a form of,
we discover a word that is a form of it. In this case, we output a relation
that's prefixed with an asterisk, telling `makeFact` to strip off the asterisk
and switch the arguments.

> makeFact :: Text -> WiktionaryTerm -> WiktionaryTerm -> WiktionaryFact
> makeFact rel from to =
>   case (unPrependChar rel) of
>     Just ('*', rev) -> WiktionaryFact rev to from
>     _               -> WiktionaryFact rel from to
>
> makeGenericFact = makeFact "RelatedTo"

Sometimes we have to parse things into WiktionaryFacts, but won't find out the
relation until after we've parsed them. In those cases, we use `"link"` as the
generic placeholder for the relation. `assignRel` replaces the relation
`"link"` with the specified `rel`, while leaving other, more specific relations
as is.

> assignRel :: Text -> WiktionaryFact -> WiktionaryFact
> assignRel rel fact@(WiktionaryFact oldRel from to) =
>   case oldRel of
>     "link" -> makeFact rel from to
>     _      -> fact


Annotations that link to terms
------------------------------

Recall that an Annotation marks up a span of text with a mapping from keys to
optional values.

We collect various Annotations on our text when parsing a Wiktionary entry.
These might indicate specific structured relations, such as "this word is an
antonym of 'down'", or they might indicate ordinary links that require further
context to understand, such as "the definition contains a link to the word
'direction'".

The annotations we get from Wiktionary entries may contain:

- `"page"`, the name of the page being linked to.

- `"namespace"`, the MediaWiki namespace of the link.

- `"language"`, the language of the dictionary entry being linked to. (As Wiktionary
  puts multiple languages on the same page, sometimes this is unknown.)

- `"rel"`, the relationship indicated by the link. A specific template could give us
  a specific relation such as `"antonym"`. Plain links have the relation `"link"`.

`linkableAnnotation` is a filter for annotations that we consider usable as links:
their "page" value is present, and their "namespace" is not. A link with a namespace
would be a link to something that's not a dictionary entry, such as an image, an audio
file, a category, or a discussion page.

> linkableAnnotation :: Annotation -> Bool
> linkableAnnotation annot = (get "page" annot /= "") && (get "namespace" annot == "")
>
> linkableAnnotations :: AnnotatedText -> [Annotation]
> linkableAnnotations atext = filter linkableAnnotation (getAnnotations atext)

`plainLinkAnnotation` further filters these annotations for those with no specific relation:
their "rel" value is "link" or is absent.

> plainLinkAnnotation :: Annotation -> Bool
> plainLinkAnnotation annot = linkableAnnotation annot && (findWithDefault "link" "rel" annot) == "link"
>
> plainLinkAnnotations :: AnnotatedText -> [Annotation]
> plainLinkAnnotations atext = filter plainLinkAnnotation (getAnnotations atext)

`languageTaggedAnnotation` filters linkable annotations for those whose
language is known.

> languageTaggedAnnotation :: Annotation -> Bool
> languageTaggedAnnotation annot = linkableAnnotation annot && (get "language" annot) /= ""
>
> languageTaggedAnnotations :: AnnotatedText -> [Annotation]
> languageTaggedAnnotations atext = filter languageTaggedAnnotation (getAnnotations atext)

You might notice that these Annotations contain most of the information we need for a
WiktionaryTerm. Is the target of a linkableAnnotation just a WiktionaryTerm?

There's one thing that's not quite aligned: if we have an annotation
with a known "senseID", it specifies a sense of the word *being defined*, not the word
being linked to. (As far as I know, there's no mechanism on Wiktionary for relating a specific
sense of one word to a specific sense of another word.)

So really, the Annotation is filling in information about *two* WiktionaryTerms that are related
by a WiktionaryFact. We take this into account with a function that converts an Annotation to a
WiktionaryFact, given the current term being defined (`thisTerm`) and the language of this
Wiktionary (`thisLang`).

One more subtlety: why don't we get `thisLang` from `thisTerm`? Those are
usually different. If the Spanish word `amigo` is being defined on the English
Wiktionary, then `thisTerm` has a language of Spanish, but `thisLang` is
English.


> annotationToFact :: Language -> WiktionaryTerm -> Annotation -> WiktionaryFact
> annotationToFact thisLang thisTerm annot =
>   let otherTerm = annotationToTerm thisLang annot
>       -- If we have a senseID, fill it in as the sense of `thisTerm`.
>       termSense = case (lookup "senseID" annot) of
>                     Nothing -> thisTerm
>                     Just sense -> thisTerm {wtSense=Just sense}
>       rel       = findWithDefault "link" "rel" annot
>   in makeFact rel termSense otherTerm

As a sub-step of this, we do need `annotationToTerm`, the function that will
create the WiktionaryTerm being linked to.

It may seem intuitive that, if the Annotation doesn't come with a language, we
would default to using `thisLang`. That would actually introduce errors. If a
word is being defined in English, that does not necessarily mean that any word
linked in the definition is an English word. It could be the same as the word
being defined, instead.

As an example, on the English Wiktionary, a definition of the Spanish word
"tengo" is `First-person singular ([[yo]]) present indicative form of
[[tener]].` Neither "yo" or "tener" here should be considered an English word,
despite that they appear in an English definition. If we have no way to
determine the language of a link, we should leave it unspecified to be inferred
later.

The reason we take in `thisLang` is because we might have to look up a language
that's given as a *section* name, such as [[tener#Spanish]]. "Spanish" is the name
of language `es` in language `en`. The language code we need to get out of that is
`es`, but we need to know that `thisLang` is `en` to extract that language code.
(We could be cleverer than this if we had the Python `langcodes` module. Alas.)

> annotationToTerm :: Language -> Annotation -> WiktionaryTerm
> annotationToTerm thisLang annot =
>   let maybeLanguage = (annotationLanguage thisLang annot) in
>     WiktionaryTerm {
>       wtText=(normalizeText (fromMaybe "und" maybeLanguage) (pageName (get "page" annot))),
>       wtLanguage=maybeLanguage,
>       wtPos=(lookup "pos" annot),
>       wtEtym=(lookup "etym" annot),
>       wtSense=(lookup "sense" annot)
>     }

These helper functions get the language of an Annotation, if possible, either
by finding the language code in the "language" value, or deciphering a
natural-language name from the "section" value.

> annotationLanguage :: Language -> Annotation -> Maybe Language
> annotationLanguage thisLang annot =
>   case (lookup "language" annot) of
>     Just language -> Just (toLanguage (fixLanguageCode language))
>     Nothing ->
>       case (lookup "section" annot) of
>         Just section -> sectionLanguage thisLang section
>         Nothing -> Nothing
>
> sectionLanguage :: Language -> Text -> Maybe Language
> sectionLanguage thisLang sectionRef =
>   case unPrependChar sectionRef of
>     Just ('#', language) -> Just (lookupLanguage thisLang language)
>     otherwise            -> Nothing

`pageName` is a helper function for getting the name of an entry being linked
to, even if it still has a section attached for some reason. (TODO: can we
clean up the processing to make this function unnecessary?)

> pageName :: Text -> Text
> pageName name = fst (splitFirst "#" name)

We might find out the sense ID from a different Annotation than the one
containing a link, so we have to handle this at the AnnotatedText level.
`findSenseID` scans through all annotations on a span of text, returning the
first sense ID it finds, if any.

> findSenseID :: AnnotatedText -> Maybe Text
> findSenseID atext = findSenseIDInList (getAnnotations atext)
>
> findSenseIDInList :: [Annotation] -> Maybe Text
> findSenseIDInList (annot:rest) =
>   case (lookup "senseID" annot) of
>     Just x -> Just x
>     Nothing -> findSenseIDInList rest
> findSenseIDInList [] = Nothing

Definition sections
-------------------

Definition sections in many languages of Wiktionary take the form of a numbered
list. For example, the English dictionary has sections labeled with a part of
speech, such as "Noun", whose contents are a numbered list of English definitions
of the word.

Here's an abridged example of the definition section for the word "thing":

    {{en-noun}}

    # That which is considered to [[exist]] as a separate [[entity]], [[object]], [[quality]] or [[concept]].
    # A [[word]], [[symbol]], [[sign]], or other [[referent]] that can be used to refer to any entity.
    # An individual object or distinct entity.
    # {{lb|en|informal}} Something that is normal or generally recognised.
    #: {{ux|en|Bacon pie? Is that a '''thing'''?}}
    # {{lb|en|legal}}
    ## Whatever can be [[own]]ed.
    ## [[corporeal|Corporeal]] [[object]].
    # {{lb|en|somewhat|_|dated}} The [[latest]] [[fad]] or [[fashion]].
    #: {{ux|en|What do you mean you don't twerk, Stacy? It's the latest '''thing'''!}}


Here, we parse the Wikitext for the numbered list, then pass its entries
on to `definitionToFacts`. If there's a parse error, we return nothing for
this section.

> parseDefinitions :: Language -> TemplateProc -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseDefinitions language tproc thisTerm text =
>   let parser = skipMiscellaneousLines (pNumberedDefinitionList tproc)
>       defs = parseOrDefault [] parser text
>   in concat (map (definitionToFacts language thisTerm) defs)

The above function is calling a parse rule that it constructs with
`skipMiscellaneousLines (pNumberedDefinitionList tproc)`.

`pNumberedDefinitionList` is the main parse rule for definitions. It takes in a
TemplateProc so it knows how to handle templates. But we want to modify the
parser so that it isn't thrown off if the definition section starts with
something that isn't a definition, such as an image or a template that
introduces the definition. That modification is done by
`skipMiscellaneousLines`.

To do this, it tries to parse each line with the inner parse rule. If that
fails, it parses one line with the generic rule `wikiTextLine`, throws it out,
and recursively runs itself to parse the rest.

> skipMiscellaneousLines :: Parser α -> Parser α
> skipMiscellaneousLines parser =
>   parser <|>
>   (newLine >> skipMiscellaneousLines parser) <|>
>   (wikiTextLine ignoreTemplates >> newLine >> skipMiscellaneousLines parser)

> pNumberedDefinitionList :: TemplateProc -> Parser [LabeledDef]
> pNumberedDefinitionList tproc = extractNumberedDefs <$> orderedList tproc "#"

Parsing an ordered list as a definition list involves running the `orderedList`
parser (from `Text.MediaWiki.WikiText`), then passing the result to
`extractNumberedDefs`.

`extractNumberedDefs` is going to associate each definition with a unique label
such as "def.1". To produce these labels, starting with the text "def", we add
1-based indices for their location in a possibly nested list:

    # def.1
    # def.2
    ## def.2.1
    ## def.2.2
    # def.3

The result of `extractNumberedDefs` is a list of LabeledDefs, which are defined
as pairs with one of these labels and the AnnotatedText of the definition.

> type LabeledDef = (Text, AnnotatedText)

First we define how to start this iterative process:

> extractNumberedDefs :: ListItem -> [LabeledDef]
> extractNumberedDefs = extractNumbered "def"
>
> extractNumbered :: Text -> ListItem -> [LabeledDef]
> extractNumbered prefix (OrderedList items) = extractNumberedIter prefix 1 items
> extractNumbered prefix _ = error "Wrong type of list"

The first case we handle is an item that introduces a sub-list. The sub-list gets
the item's label as its prefix.

After that, we handle normal items, sub-lists that aren't OrderedLists which we
ignore, and the base case at the end of the list.

> extractNumberedIter :: Text -> Int -> [ListItem] -> [LabeledDef]
> extractNumberedIter prefix counter list =
>   let newPrefix = mconcat [prefix, ".", cs (show counter)]
>   in case list of
>     ((Item item):(OrderedList items):rest) -> (newPrefix, item):(
>                                                 (extractNumberedIter newPrefix 1 items)
>                                                 ++ (extractNumberedIter prefix (counter + 1) rest)
>                                                 )
>     ((Item item):rest)                     -> (newPrefix, item):(extractNumberedIter prefix (counter + 1) rest)
>     _:rest                                 -> extractNumberedIter prefix counter rest
>     []                                     -> []

In German and some other Wiktionaries that have followed its lead, lists have
specifically numbered word senses instead of automatic numbering. Instead of
starting with `#`, for example, a definition line starts with `:[1]`.

TODO: We don't bother parsing sub-definitions in this format. If the line after
definition 1 starts with, for example, `::[a]`, we could interpret this as a
definition labeled "1a", but we currently don't.

`pLabeledDefinitionList` is a parse rule for these definitions, similar to
`pNumberedDefinitionList` above.

> pLabeledDefinitionList :: TemplateProc -> Parser [LabeledDef]
> pLabeledDefinitionList tproc = extractLabeledDefs <$> indentedList tproc ":"

`extractLabeledDefs` scans through the items in an IndentedList, and parses them
as definitions with optional labels, using the parse rule `pLabeledItem`.

A definition can have zero or more labels. If it has zero labels, then it is
probably the sole definition of the word, so we make one LabeledDef where we
keep the definition and use the empty string as its label.

If it has one or more labels, we make a LabeledDef coresponding to each label,
using `adjustLabel`, defined below.

> extractLabeledDefs :: ListItem -> [LabeledDef]
> extractLabeledDefs (IndentedList items) = mconcat (map extractLabeledDefItem items)
> extractLabeledDefs _ = error "Wrong type of list"
>
> extractLabeledDefItem :: ListItem -> [LabeledDef]
> extractLabeledDefItem (Item item) =
>   case parseOnly pLabeledItem (getText item) of
>     Left err -> []
>     Right (labelList, defn) ->
>       case labelList of
>         []                -> [("", item)]
>         nonEmptyLabelList -> [adjustLabel label defn item | label <- nonEmptyLabelList]
> extractLabeledDefItem _ = []

`adjustLabel` transforms an AnnotatedText representing a list item for the fact
that we've parsed a bracketed part at the beginning as a sense label. The
existing AnnotatedText has no `senseID`, and contains the bracketed part as
literal text.

We convert it into a new AnnotatedText whose `senseID` is named after the
label, and whose text is the remaining text, passed in as `defn`.  The other
annotations are preserved.

> adjustLabel :: Text -> Text -> AnnotatedText -> LabeledDef
> adjustLabel senseLabel defn atext =
>   let senseID     = "def." ⊕ senseLabel
>       annotations = (singletonMap "senseID" senseID):(getAnnotations atext)
>       atext'      = annotate annotations defn
>   in (senseID, atext')
>
> extractLabeledItems :: ListItem -> [AnnotatedText]
> extractLabeledItems items = map snd (extractLabeledDefs items)

Items in definition lists, and their corresponding entries in sections that
describe particular relations, can have complex lists of label numbers. For
example, the entry that translates the German word "gehen" into English looks
like:

    [1] {{Ü|en|walk}}; [1–3, 7, 13] {{Ü|en|go}}; ...

Here, sense 1 of "gehen" is translated as "walk", but all of senses 1, 2, 3, 7,
and 13 are translated as "go".

`pLabeledItem` parses one of these items as a list of zero or more labels, followed
by the remaining plain text of the definition.

> pLabeledItem :: Parser ([Text], Text)
> pLabeledItem = do
>   labels <- pOptionalBracketedLabels
>   text <- takeText
>   return (labels, text)

`pOptionalBracketedLabels` returns a list of labels, as text, if they are present,
or an empty list if they are not.

> pOptionalBracketedLabels :: Parser [Text]
> pOptionalBracketedLabels = pBracketedLabels <|> return []

`pBracketedLabels` parses a list of labels, assuming they're there. If the labels
aren't there, it fails (in which case it's the job of `pOptionalBracketedLabels`
to return an empty list).

> pBracketedLabels = do
>   string "["
>   labels <- pLabels
>   string "]"
>   skipSpace
>   return labels

The interior of a label list is either single labels or ranges, separated
by commas and optional whitespace. For example: `1, 2a, 4-6`. We find these
comma-separated groups of labels, and concatenate them into one big list.

> pLabels :: Parser [Text]
> pLabels = mconcat <$> sepBy1 pCommaSeparatedLabel (char ',' >> skipSpace)
>
> pCommaSeparatedLabel :: Parser [Text]
> pCommaSeparatedLabel = pLabelRange <|> pSingleLabel

A single label is usually a whole number, but sometimes there are
sub-definitions that use letters, such as definition 2 being listed as `[2a]`
and `[2b]`. If the sub-definitions go beyond letter `j`, it's getting a bit
ridiculous, though.

Instead of the `do` block, this could have been written more concisely and
more confusingly as `pure <$> textWith "0123456789abcdefghij"`. Typical
Haskell programmers may wonder why I didn't do it that way, and others
may wonder why you *would* do it that way.

> pSingleLabel :: Parser [Text]
> pSingleLabel = do
>   label <- textWith "0123456789abcdefghij"
>   return [label]

A label range is made of whole-numbered labels (parsed using `decimal`)
separated by some kind of dash. When we parse one, we return it as a list of
the numeric values that it spans.

We don't deal with ranges involving letters, such as `[2a-d]`.

> pLabelRange = do
>   startNum <- decimal
>   textWith "-–—"
>   endNum <- decimal
>   return (map tshow [startNum..endNum])


When we get a LabeledDef value -- that is, an AnnotatedText for a definition, with
a text label -- the next thing we want to do is convert it into some number of
WiktionaryFacts.

First we find the word sense that the definitions should use. Often this comes
from the label. But if a template within the definition has specifically provided
a SenseID, we use that first.

`mplus` is an operator on monads. Here we're applying it to the Maybe monad,
where it returns the first `Just` value, to prioritize the sources that might
provide a sense label.

`thisTerm` is the term being defined, which we refine by filling in its `wtSense`
with the sense label we found.

As for the actual text of the definition, we split it into multiple independent
definitions by running the `pDefinitionText` parser on it, defined next.

The WiktionaryFacts that we output are a fact built with `makeDefinitionFact` for
each piece of the definition text, plus a fact built with `annotationToFact`
for each usable annotation on that text.

> definitionToFacts :: Language -> WiktionaryTerm -> LabeledDef -> [WiktionaryFact]
> definitionToFacts language thisTerm defPair =
>   let defText = snd defPair
>       -- get a sense either from the SenseID annotation, or failing that,
>       -- from the label that comes with the definition
>       defSense = mplus (findSenseID defText) (nonEmpty (Just (fst defPair)))
>       termSense = thisTerm {wtSense=defSense}
>       defPieces = splitDefinition (stripSpaces (getText defText))
>   in (map (makeDefinitionFact termSense language) defPieces)
>      ⊕ (map (annotationToFact language termSense) (linkableAnnotations defText))

`makeDefinitionFact` makes a WiktionaryFact out of readable text in a
definition.  It takes in the term being defined and the language it's being
defined in, and then the text of the definition. It outputs a WiktionaryFact
whose `rel` is "definition", pointing from the term being defined, to a term
made out of the definition text in the appropriate language.

> makeDefinitionFact :: WiktionaryTerm -> Language -> Text -> WiktionaryFact
> makeDefinitionFact termSense language definition =
>   makeFact "definition" termSense (simpleTerm language definition)

`splitDefinition` runs the `pDefinitionText` parser as a separate parsing
stage.  This parser should be able to parse whatever it's given as a
definition, so if it fails, it's not just a situation where the Wiktionary
parser should backtrack, it's a runtime error that should be raised.

> splitDefinition :: Text -> [Text]
> splitDefinition definition =
>   if definition == "" then []
>   else
>     case parseOnly pDefinitionText definition of
>       Right results -> results
>       Left err -> error err

Parsing the language of definitions
-----------------------------------

Definitions on Wiktionary often define the same thing in multiple ways. These sub-definitions
are usually separated by semicolons. For example, the first definition of "vector" is:

    A directed quantity, one with both magnitude and direction; the signed difference between two points.

When you split at the semicolon, you get two separate ways to describe this word sense.

Some definitions, especially of words in other languages, are just a list of synonyms separated by
commas. The definition of the Russian word "специалист" is:

    specialist, expert

We also want to parse that form of definition. However, we restrict it to a list of single
words, because we don't want every comma that appears in a definition (such as the one above) to
act as a place to split the definition.

This could probably be done better. When the Japanese word "車" is defined as
"a car, an automobile, a carriage, a cart", we fail to recognize that as four
definitions joined by commas, because they contain spaces. Fortunately, each of
those words is also a link, which `annotationToFact` will be able to turn into
a WiktionaryFact, so we do get the four separate definitions anyway.

So to parse a definition, we first look for single words separated by commas.
Failing that, we look for sub-definitions separated by semicolons, with an
optional period at the end.  If neither parser works (perhaps the definition is
multiple sentences, so splitting it at semicolons would be likely to be wrong),
we just return the entire text as a single definition.

> pDefinitionText :: Parser [Text]
> pDefinitionText = (pDefCommas <|> pDefSemicolons <|> pDefAnything)
>
> pCommaItem     = textWithout " ,;:."
> pSemicolonItem = textWithout ";."

To parse the definition as comma-separated, it must be made of items that parse as
`pCommaItem`, separated by `", "`. This must encompass the entire definition, so at
the end we make sure to parse `endOfInput` (discarding its meaningless value).

> pDefCommas :: Parser [Text]
> pDefCommas = sepBy1 pCommaItem (string ", ") <* endOfInput

Parsing the definition as semicolon-separated is similar, but we have the
additional thing that there's an optional period at the end. That makes the
rule complicated enough that we break it into steps using a `do` block.

> pDefSemicolons :: Parser [Text]
> pDefSemicolons = do
>   items <- sepBy pSemicolonItem (string "; ")
>   option '.' (char '.')
>   endOfInput
>   return items

`pDefAnything` parses literally anything and wraps it in a list. We can get
away with this because it's running in a sub-parser. It won't consume the rest
of the whole page, just the whole definition.

> pDefAnything :: Parser [Text]
> pDefAnything = do
>   text <- takeText
>   return [text]


Relation sections
-----------------

Multiple languages of Wiktionary have sections for particular relations, such as "Synonyms",
"Antonyms", and the generic "Related terms". Such a section could look like this (for the
English word *loud*):

    ====Antonyms====
    * {{sense|sound}} {{l|en|quiet}}, {{l|en|soft}}
    * {{sense|person}} {{l|en|quiet}}

We want to extract the links from such a bulleted list and turn them into WiktionaryFacts.
The general idea of what we want to do is the same across languages, but the details vary.
So in this code, we define a structure called `RelationSectionInfo` that encapsulates
the language-specific details of the process, and pass it as an argument to functions that
do the work in general.

The details we need to keep track of are:

- The Language that we're handling
- The template procedure for handling templates in that language
- A parse rule for parsing one item from the relation section

> data RelationSectionInfo = RelationSectionInfo {
>   rsLanguage :: Language,                                 -- the language to be parsed
>   rsTemplateProc :: TemplateProc,                         -- the template procedure to use
>   rsItemRule :: (TemplateProc -> Parser [AnnotatedText])  -- how to parse items
> }

`parseRelation` gets the result of parsing an entire relation section. It takes these arguments:

- The RelationSectionInfo for the language
- The Wikitext to be parsed
- The term being defined
- The relation name that will go in the "rel" field of the WiktionaryFacts

and it produces a list of WiktionaryFacts, or the empty list if it fails to parse. Its return value
is a plain value, not wrapped in a Parser.

This function passes most of its arguments on to the actual parse rule `pRelationSection`.

> parseRelation :: RelationSectionInfo -> Text -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseRelation rsInfo rel thisTerm text =
>   parseOrDefault [] (pRelationSection rsInfo rel thisTerm) text

`pRelationSection` is the parse rule. It repeatedly applies the `rsItemRule`
(or ignores a line), converts the AnnotatedTexts it returns to WiktionaryFacts
using `entryToFacts` (defined below), and applies the given `rel` to these
facts. It uses `mconcat` to flatten together multiple lists of results as it
goes.

> pRelationSection :: RelationSectionInfo -> Text -> WiktionaryTerm -> Parser [WiktionaryFact]
> pRelationSection rsInfo rel thisTerm =
>   let tproc = (rsTemplateProc rsInfo)
>       language = rsLanguage rsInfo
>       itemRule = rsItemRule rsInfo
>   in map (assignRel rel)
>     <$> mconcat
>     <$> map (entryToFacts language thisTerm)
>     <$> mconcat
>     <$> many ((itemRule tproc) <|> pRelationIgnored)

pRelationItem is a sensible default function to pass as `rsItemRule`: it gets the AnnotatedText
contents of one list item introduced by the bullet `*`, limited to a single link.

> pRelationItem :: TemplateProc -> Parser [AnnotatedText]
> pRelationItem tproc =
>   extractFirstLink <$> listItem tproc "*"

Lines that don't match the rule for parsing items -- such as templates outside
of the list, or blank lines -- are skipped, producing no results.

> pRelationIgnored :: Parser [AnnotatedText]
> pRelationIgnored = wikiTextLine ignoreTemplates >> newLine >> return []

`entryToFacts` is a simpler version of `definitionToFacts` that's not specialized for definitions.
It converts the annotations on an AnnotatedText to some number of WiktionaryFacts, applying
a word sense ID if it exists.

> entryToFacts :: Language -> WiktionaryTerm -> AnnotatedText -> [WiktionaryFact]
> entryToFacts thisLang thisTerm defText =
>   let defSense  = findSenseID defText
>       termSense = thisTerm {wtSense=defSense}
>       annots    = plainLinkAnnotations defText
>   in map (annotationToFact thisLang termSense) annots


The translation section
-----------------------

Most of the work involved in parsing translation sections is fundamentally
the same between different languages. However, details are different between
languages, most notably the names of the templates involved.

If we passed in all these details as arguments to `parseTranslations`, the type
of `parseTranslations` and its helper functions would be horrifying. Instead,
like we did for relation sections, we'll group them together into a
TranslationSectionInfo struct.

> data TranslationSectionInfo = TranslationSectionInfo {
>   tsLanguage :: Language,             -- the language to be parsed
>   tsTemplateProc :: TemplateProc,     -- the template procedure to use
>   tsStartRule :: Parser (Maybe Text), -- a parser for the start of a translation list
>   tsIgnoreRule :: Parser (),          -- a parser for lines to ignore
>   tsEndRule :: Parser ()              -- a parser for the end of a translation list
> }

As with relation sections above, `parseTranslations` runs the parse rule we're
about to define and extracts its list of results (if any). Its arguments are:

- The TranslationSectionInfo for the language
- The term being defined
- The Wikitext to be parsed

It returns (as a plain value, not wrapped in a Parser) the list of facts it
extracts, if any.

> parseTranslations :: TranslationSectionInfo -> WiktionaryTerm -> Text -> [WiktionaryFact]
> parseTranslations tsInfo thisTerm text =
>   parseOrDefault [] (pTranslationSection tsInfo thisTerm) text

`pTranslationSection` parses one or more translation groups (defined below).

> pTranslationSection :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationSection tsInfo thisTerm = concat <$> many1 (pTranslationGroup tsInfo thisTerm)

A translation group is a portion of the "Translations" section that all applies to the same
word sense. It's delimited by a start template and an end template. The start template might
return a word sense, or might return Nothing.

This parser runs `tsStartRule` and possibly gets a word sense from it, then
runs `pTranslationGroupBody` with that word sense to get the translations it
will actually return, and finally runs `tsEndRule`. It skips blank lines before
and after.

> pTranslationGroup :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationGroup tsInfo thisTerm = do
>   optionalTextChoices [newLine]
>   maybeSense <- tsStartRule tsInfo
>   let senseTerm = thisTerm {wtSense=maybeSense}
>   items <- pTranslationGroupBody tsInfo senseTerm
>   tsEndRule tsInfo
>   optionalTextChoices [newLine]
>   return items

The body of a translation group is a list of translation items, defining translations of a
particular sense of a WiktionaryTerm (or an ambiguous WiktionaryTerm whose sense is Nothing).

We look for one or more lines that are either matches for `pTranslationItem` or for `tsIgnoreRule`.
We concatenate together the AnnotatedTexts they produce, then run `extractTranslations` on them
to turn them into WiktionaryFacts.

> pTranslationGroupBody :: TranslationSectionInfo -> WiktionaryTerm -> Parser [WiktionaryFact]
> pTranslationGroupBody tsInfo thisTerm =
>   mconcat <$>
>     map (extractTranslations (tsLanguage tsInfo) thisTerm) <$>
>     mconcat <$>
>     many1 (pTranslationItem tsInfo <|> (tsIgnoreRule tsInfo >> return []))
>
> extractTranslations :: Language -> WiktionaryTerm -> AnnotatedText -> [WiktionaryFact]
> extractTranslations language thisTerm atext =
>   let translationAnnotations = filter isTranslation (getAnnotations atext) in
>     map (annotationToFact language thisTerm) translationAnnotations
>
> isTranslation :: Annotation -> Bool
> isTranslation a = lookup "rel" a == Just "translation"

To get translation candidates out of a bunch of bullet points, we need to find
the items the bullet-pointed list entry contains.

There may be multiple of them, because some translation entries are nested
lists -- multiple kinds of translations for the same language, for example.
`extractTopLevel` turns these items into a flat list.

> pTranslationItem :: TranslationSectionInfo -> Parser [AnnotatedText]
> pTranslationItem tsInfo = extractTopLevel <$> listItem (tsTemplateProc tsInfo) "*"

The `tsIgnoreRule` has to be written for each language, as a parser that takes in lines
and returns `()`. As one simple case, `pBlankLine` parses a blank line and returns `()`.

> pBlankLine :: Parser ()
> pBlankLine = newLine >> return ()


Looking up sections
-------------------

To understand what we're parsing, we need to understand what kind of section it is.

Sections don't necessarily come out and *tell* you what kind of section they
are. For example, it would be great if the section that contains definitions
were named **Definitions**.  However, in the grand Wiktionary tradition of
conflating semantics with presentation, the heading on that section is the part
of speech, such as **Noun** or **Adverb**.

`findHeading` handles one case of this problem: given our current stack of
headings (the hierarchy of headings leading to a section), we want to find the first
one (if any) that matches a known set, such as names of parts of speech.

Simple example: when `findHeading (HashSet ["Noun", "Verb", "Adjective"])` is run
on the list `["English", "Etymology 1", "Noun"]`, it will return `Just "Noun"`.
When run on `["English", "Etymology 1"]`, it will return `Nothing`.

> findHeading :: HashSet Text -> [Text] -> Maybe Text
> findHeading choices headings =
>   let filtered = filter (∈ choices) headings
>   in headMay filtered

`findPrefixedHeading` finds the first heading in a stack of headings that begins
with a particular prefix, such as "Etymology ". If it returns a result, the
result is a `Just` containing the rest of the heading, such as "1".

> findPrefixedHeading :: Text -> [Text] -> Maybe Text
> findPrefixedHeading prefix headings =
>   let filtered = filter (isPrefixOf prefix) headings
>       mapped   = map (drop (length prefix)) filtered
>   in headMay mapped


Transforming templates
----------------------

Many of the template functions we define will involve converting a Template
value into an AnnotatedText.

These are similar but not quite the same. A Template indicates what the
Wikitext syntax *says*, but the AnnotatedText output says what it *means*.
Converting one to the other will involve handling specific values based on
how it will be used.

Their types are structured somewhat differently, as well. A Template is an
association list from Text parameters to Text values. An AnnotatedText contains
visible text, plus any number of Annotations, which are maps from Text keys to
Text values.

But very often, we want to turn Template parameters directly into keys in an
Annotation, or into the visible text of the AnnotatedText.  Describing these
operations repeatedly would be quite verbose. So what we're defining here is a
little language for turning Templates into AnnotatedTexts.

To get some convenient `do` syntax for this, we use `put`, which lets us
assemble values using a monad called Writer. The values are accumulated into a
sort of state, using their `mconcat` operation, because the state is in fact
a monoid. Yes, we're using a monad to build a monoid. I promise this will be
really useful for templates.

In particular, here, the monoid we're building is a map that serves as an
Annotation.

At the end, we can return a separate value that isn't part of that accumulation.
We use this for the text that gets annotated.

Here's an example (this code doesn't actually go here) of how we'll use `put`
and `annotationBuilder`:

    handlePOSTemplate t = annotationBuilder $ do
      put "pos" (partOfSpeechMap (get "1" t))
      put "language" (getLanguage "2" t)
      return "POS"

We get argument "1" from the template, run that through a `partOfSpeechMap`,
and assign the result as the "pos" value of our Annotation. We do something
similar with a function called `getLanguage`, putting the result in the
"language" value. As the text that actually gets annotated, we return the
placeholder text "POS".

So `put` is our monoid-monad thing that uses `writer` to assemble a state using
do-notation. We also want the values to be a Monoid, so we can check if they're
empty. If a value is empty, we do nothing to the state (we append `ø`). If the
value is present, we append a map that just maps `key` to `value`, and the
effect of that is to map `key` to `value` in the state.

> put :: (IsMap map, Monoid (MapValue map), Eq (MapValue map)) => ContainerKey map -> MapValue map -> Writer map (MapValue map)
> put key value =
>   if (value == ø)
>     then writer (value, ø)
>     else writer (value, singletonMap key value)

If that was obscure, don't worry about it.

`annotationBuilder` is how we use that monad to create an AnnotatedText.
`runWriter` runs the monad and gives us a pair of its return value and its
accumulated state. The state is our singular Annotation, and the return value
is our text.

> annotationBuilder :: Writer Annotation Text -> AnnotatedText
> annotationBuilder m =
>   let (text, anno) = runWriter m in
>     annotate [anno] text

Let's add some shorthand to our little language. Instead of just using `put`
and `return`, we're going to write operations that look like this:

    handleDerivationTemplate t = annotationBuilder $ do
      put "rel" "*derived/etym"
      adapt "language" arg2 t
      adapt "page" arg3 t
      visible arg3 t

The thing we want to `put` in the annotation will often be something that we
`get` or `getPrioritized` from the template, so we combine `getPrioritized` and
`put` into one operation called `adapt`. It looks at the keys of the template
given by `keySources`, and puts the first one it finds in the Annotation value
with the key `keyTarget`.

Sometimes we know exactly which argument to look in and don't need a priority
order, so here are some lists of a single key:

> arg1, arg2, arg3 :: [Text]
> arg1 = ["1"]
> arg2 = ["2"]
> arg3 = ["3"]

So in the `handleDerivationTemplate` example above, we get arg 2 of the Template
and make it the `language` of the annotation, and get arg 3 of the Template and
make it the `page`, using `adapt`. Here's its definition.

> adapt :: Text -> [Text] -> Template -> Writer Annotation Text
> adapt keyTarget keySources = (put keyTarget) . (getPrioritized keySources)

`visible` is a combination of `getPrioritized` and `return` -- it similarly
looks up a list of keys, and makes the first one it finds the text result.

> visible :: [Text] -> Template -> Writer Annotation Text
> visible keySources = return . (getPrioritized keySources)

`invisible`, then, means that the text of the template is considered to be the empty
string -- we're using it only for its Annotations.

> invisible :: Writer Annotation Text
> invisible = return ""

Unbounded numbers of arguments
------------------------------
There are now templates that take an unbounded number of arguments and make
a table out of them, such as {{der3}} on en.wiktionary.org.

`handleUnboundedTemplate` builds a template-handler that handles these
templates, given the relation to extract and the default language (usually
the language the Wiktionary is in).

> handleUnboundedTemplate :: Text -> Text -> Template -> AnnotatedText
> handleUnboundedTemplate rel defaultLanguage t =
>   let language = findWithDefault defaultLanguage "lang" t
>       keys = [cs (show n) | n <- [1..200]]
>       entries = map stripGloss (getAll keys t)
>       annots = [annotationFromList [("rel", rel),
>                                     ("language", language),
>                                     ("page", text)] | text <- entries]
>   in annotate annots ""
>
> stripGloss :: Text -> Text
> stripGloss text = fst (splitFirst ":" text)

An entry point for parsing an entire page
-----------------------------------------

The `handleFileJSON` function isn't actually used anywhere except for
experimentation, but it is similar to what the `WiktionaryParser` command needs
to do. I include it here to illustrate what we need to accomplish.

This function takes in a file containing a Wiktionary page, as Wikitext that
possibly includes HTML. It extracts a list of facts from it using a
`languageParser` that's defined separately for each language of Wiktionary
being parsed. (We'll see those modules later.) The extracted facts are then
encoded as JSON structures and sent to standard out.

The `languageParser` is a function that takes in a page title (which is
important because it tells us what word is being defined) and the non-HTML
Wikitext of that page, and outputs a list of WiktionaryFacts.

The WiktionaryFacts are converted to output by sending them through
the JSON `encode`, then through the all-purpose text type converter `cs`
(I really tried to name something more specific but I failed), and then through
`println`. `mapM_` applies this chain of functions to each of the results,
in order, using the IO monad.

> handleFileJSON :: (Text -> Text -> [WiktionaryFact]) -> Text -> FilePath -> IO ()
> handleFileJSON languageParser title filename = do
>   contents <- (readFile filename) :: IO ByteString
>   let fromHTML = extractWikiTextFromHTML contents
>   mapM_ (println . cs . encode) (languageParser title fromHTML)
