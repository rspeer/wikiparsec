#!/bin/sh
SOURCE_FILES="README.md WikiPrelude.lhs Text/Language/Normalize.lhs Data/LanguageType.lhs \
Data/LanguageNames.lhs Text/MediaWiki/XML.lhs Text/MediaWiki/HTML.lhs Text/MediaWiki/Sections.lhs \
Text/SplitUtils.lhs Text/MediaWiki/AnnotatedText.lhs Text/MediaWiki/WikiText.lhs"
pandoc -S -c docs.css --toc -o docs/wikiparsec.html -f markdown+lhs $SOURCE_FILES
