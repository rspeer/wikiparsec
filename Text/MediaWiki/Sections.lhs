> {-# LANGUAGE OverloadedStrings #-}
>
> module Text.MediaWiki.Sections where
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.Char8 as Char8
> import Data.ByteString (ByteString)
> import Text.Parsec.Pos
> import Text.Parsec.Prim
> import Text.Parsec.Combinator
> import Text.Parsec.Error
> import Control.Applicative ((<$>))
> import Data.Maybe (fromJust)


Data structures
===============

First we'll break the text into lines. Then we group these lines into
individual sections with their headings. Finally, we step through the
sections, converting them into contextualized WikiSection objects that know
about their entire stack of headings.

Here we define the data structures representing the outputs of these various
steps.

> data TextLine = Heading Int ByteString | Plain ByteString deriving (Eq, Show)
>
> isHeading :: TextLine -> Bool
> isHeading (Heading _ _) = True
> isHeading _             = False
>
> getText :: TextLine -> ByteString
> getText (Plain text) = text
>
> data SingleSection = SingleSection {
>   ssLevel :: Int,
>   ssHeading :: ByteString,
>   ssContent :: ByteString
> } deriving (Eq, Show)
>
> data WikiSection = WikiSection {
>   headings :: [ByteString],
>   content :: ByteString
> } deriving (Eq, Show)


Reading lines
=============

This is a kind of lexer for the section parser. We sort the lines of the page
into two types: headings and non-headings.

> stripSpaces :: ByteString -> ByteString
> stripSpaces = Char8.reverse . stripSpacesFront . Char8.reverse . stripSpacesFront
> stripSpacesFront = Char8.dropWhile (== ' ')

> readLines :: ByteString -> [TextLine]
> readLines text = map parseTextLine (Char8.lines text)
>
> parseTextLine :: ByteString -> TextLine
> parseTextLine text =
>   if Char8.isPrefixOf "----" text
>     then (Plain "")   -- remove horizontal rules
>     else
>       let (innerText, level) = headingWithLevel (stripSpaces text)
>       in  (if level == 0 then (Plain text) else (Heading level (stripSpaces innerText)))
>
> headingWithLevel :: ByteString -> (ByteString, Int)
> headingWithLevel text =
>   if (BS.length text) > 1 && Char8.isPrefixOf "=" text && Char8.isSuffixOf "=" text
>     then let innerText               = trim text
>              (finalText, innerLevel) = headingWithLevel innerText
>          in  (finalText, innerLevel + 1)
>     else (text, 0)

`trim` is a helper that takes in a text of length at least 2, and strips off
its first and last character.

> trim :: ByteString -> ByteString
> trim = Char8.init . Char8.tail


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

> pPlainLine :: LineParser ByteString
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
>   return (SingleSection { ssLevel = level, ssHeading = name, ssContent = Char8.unlines textLines })


Converting sections
===================

Here's how we convert a list of SingleSections into a list of contextualized
WikiSections.

> convertSections :: [SingleSection] -> [WikiSection]
> convertSections = processSectionHeadings ["top"]
>
> processSectionHeadings :: [ByteString] -> [SingleSection] -> [WikiSection]
> processSectionHeadings headingStack [] = []
> processSectionHeadings headingStack (sec:rest) =
>   let sec' = (applyHeadings headingStack sec)
>       heds = (headings sec')
>   in  (sec':(processSectionHeadings heds rest))
>
> applyHeadings :: [ByteString] -> SingleSection -> WikiSection
> applyHeadings headingStack sec =
>   let heds = (take ((ssLevel sec) - 1) headingStack) ++ [ssHeading sec]
>   in  WikiSection { headings = heds, content = ssContent sec }


Parsing the whole page
======================

It's convenient for us if all text is in a section. The text that precedes any
section headings is effectively in a level-1 section called "top". Let's just
add the heading for it before we scan its lines.

> preparePage :: ByteString -> [TextLine]
> preparePage text = readLines (BS.append "=top=\n" text)
>
> pPage :: LineParser [WikiSection]
> pPage = convertSections <$> many pSection
>
> parsePageIntoSections :: ByteString -> [WikiSection]
> parsePageIntoSections text = 
>   case (parse pPage "" (preparePage text)) of
>     Left err       -> []
>     Right sections -> sections
