{-# LANGUAGE OverloadedStrings #-}

-- TODO: clean up unnecessary imports
import Test.HUnit
import Text.MediaWiki.WikiText
import Text.MediaWiki.Wiktionary.Base
import Text.MediaWiki.Wiktionary.English
import Text.MediaWiki.AnnotatedString (Annotation, AnnotatedString, makeLink)
import Text.MediaWiki.Templates (noTemplates, idTemplate, useArg)
import qualified Text.MediaWiki.AnnotatedString as A
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Either (rights)

testExtract :: (Eq a, Show a) => (ByteString -> a) -> ByteString -> a -> Test
testExtract func input output = (UTF8.toString input) ~: (func input) ~?= output

defnTests = [
    testExtract (enParseDefinition (simpleTerm "en" "test"))
                "# {{senseid|en|first}} definition 1\n# {{senseid|en|second}} definition 2\n# definition 3"
                [WiktionaryRel {relation = "definition", fromTerm = (simpleTerm "en" "test") {sense=Just "first"}, toTerm = simpleTerm "en" "definition 1"},
                 WiktionaryRel {relation = "definition", fromTerm = (simpleTerm "en" "test") {sense=Just "second"}, toTerm = simpleTerm "en" "definition 2"},
                 WiktionaryRel {relation = "definition", fromTerm = (simpleTerm "en" "test") {sense=Just "def.3"}, toTerm = simpleTerm "en" "definition 3"}]
    ]

tests = test (defnTests)

main :: IO ()
main = void (runTestTT tests)
