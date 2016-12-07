#!/bin/sh
SOURCE_FILES="WikiPrelude.lhs Text/Language/Normalize.lhs"
pandoc -S -c docs.css --toc -o docs/wikiparsec.html -f markdown+lhs $SOURCE_FILES
