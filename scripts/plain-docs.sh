#!/bin/sh
SOURCE_FILES="README.md WikiPrelude.lhs Text/SplitUtils.lhs Text/Language/Normalize.lhs Data/LanguageType.lhs \
Data/LanguageNames.lhs Text/MediaWiki/XML.lhs Text/MediaWiki/HTML.lhs Text/MediaWiki/Sections.lhs \
Text/MediaWiki/AnnotatedText.lhs Text/MediaWiki/WikiText.lhs Text/MediaWiki/Templates.lhs \
Text/MediaWiki/Wiktionary/Base.lhs Text/MediaWiki/Wiktionary/English.lhs"
pandoc -S -c docs.css -o docs/wikiparsec.md.lhs -f markdown+lhs -t markdown+lhs $SOURCE_FILES
