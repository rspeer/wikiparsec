> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
>
> module Text.MediaWiki.Sections where
> import WikiPrelude hiding (many)
> import Text.Parsec.Pos
> import Text.Parsec.Prim
> import Text.Parsec.Combinator
> import Text.Parsec.Error


Data structures
===============

First we break the text into lines. Then we group these lines into
individual sections with their headings. Finally, we step through the
sections, converting them into contextualized WikiSection objects that know
about their entire stack of headings.

Here we define the data structures representing the outputs of these various
steps.

> data TextLine = Heading Int Text | Plain Text deriving (Eq, Show)
>
> isHeading :: TextLine -> Bool
> isHeading (Heading _ _) = True
> isHeading _             = False
>
> getText :: TextLine -> Text
> getText (Plain text) = text
>
> data SingleSection = SingleSection {
>   ssLevel :: Int,
>   ssHeading :: Text,
>   ssContent :: Text
> } deriving (Eq, Show)
>
> data WikiSection = WikiSection {
>   headings :: [Text],
>   content :: Text
> } deriving (Eq, Show)


Reading lines
=============

This is a kind of lexer for the section parser. We sort the lines of the page
into two types: headings and non-headings.

> stripSpaces :: Text -> Text
> stripSpaces = reverse . stripSpacesFront . reverse . stripSpacesFront
> stripSpacesFront = dropWhile (== ' ')

> readLines :: Text -> [TextLine]
> readLines text = map parseTextLine (lines text)
>
> parseTextLine :: Text -> TextLine
> parseTextLine text =
>   if isPrefixOf "----" text
>     then (Plain "")   -- remove horizontal rules
>     else
>       let (innerText, level) = headingWithLevel (stripSpaces text)
>       in  (if level == 0 then (Plain text) else (Heading level (stripSpaces innerText)))
>
> headingWithLevel :: Text -> (Text, Int)
> headingWithLevel text =
>   case trimEquals text of
>     Just trimmed ->
>       let (finalText, innerLevel) = headingWithLevel trimmed in
>         (finalText, innerLevel + 1)
>     Nothing -> (text, 0)

`trimEquals` takes in a text, and if it can trim an equals sign from each end,
returns the trimmed result. Otherwise, it returns Nothing.

> trimEquals :: Text -> Maybe Text
> trimEquals text =
>   case stripPrefix "=" text of
>     Nothing -> Nothing
>     Just x  -> stripSuffix "=" x


A line-by-line parser
=====================

> type LineParser = Parsec [TextLine] ()

Here's some boilerplate to help Parsec understand that our tokens are lines:

> matchLine :: (TextLine -> Bool) -> LineParser TextLine
> matchLine pred =
>   let showLine = show
>       testLine line = if pred line then Just line else Nothing
>       nextPos pos x xs = updatePosLine pos x
>   in  tokenPrim showLine nextPos testLine
>
> updatePosLine :: SourcePos -> TextLine -> SourcePos
> updatePosLine pos _ = incSourceLine pos 1

Now we can use it to define two token-matching parsers:

> pPlainLine :: LineParser Text
> pPlainLine = getText <$> matchLine (not . isHeading)
>
> pHeadingLine :: LineParser TextLine
> pHeadingLine = matchLine isHeading


Parsing sections
================

> pSection :: LineParser SingleSection
> pSection = do
>   Heading level name <- pHeadingLine
>   textLines <- many pPlainLine
>   return (SingleSection { ssLevel = level, ssHeading = name, ssContent = unlines textLines })


Converting sections
===================

Here's how we convert a list of SingleSections into a list of contextualized
WikiSections.

> convertSections :: [SingleSection] -> [WikiSection]
> convertSections = processSectionHeadings ["top"]
>
> processSectionHeadings :: [Text] -> [SingleSection] -> [WikiSection]
> processSectionHeadings headingStack [] = []
> processSectionHeadings headingStack (sec:rest) =
>   let sec' = (applyHeadings headingStack sec)
>       heds = (headings sec')
>   in  (sec':(processSectionHeadings heds rest))
>
> applyHeadings :: [Text] -> SingleSection -> WikiSection
> applyHeadings headingStack sec =
>   let heds = (take ((ssLevel sec) - 1) headingStack) ++ [ssHeading sec]
>   in  WikiSection { headings = heds, content = ssContent sec }


Parsing the whole page
======================

It's convenient for us if all text is in a section. The text that precedes any
section headings is effectively in a level-1 section called "top". Let's just
add the heading for it before we scan its lines.

> preparePage :: Text -> [TextLine]
> preparePage text = readLines ("=top=\n" <> text)
>
> pPage :: LineParser [WikiSection]
> pPage = convertSections <$> many pSection
>
> parsePageIntoSections :: Text -> [WikiSection]
> parsePageIntoSections text =
>   case (parse pPage "" (preparePage text)) of
>     Left err       -> []
>     Right sections -> sections

