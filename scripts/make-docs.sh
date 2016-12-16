#!/bin/sh
SOURCE_FILES="README.md WikiPrelude.lhs Text/Language/Normalize.lhs Data/LanguageType.lhs Data/LanguageNames.lhs Text/MediaWiki/XML.lhs"
pandoc -S -c docs.css --toc -o docs/wikiparsec.html -f markdown+lhs $SOURCE_FILES
