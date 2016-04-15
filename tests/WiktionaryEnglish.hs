{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- TODO: clean up unnecessary imports
import WikiPrelude
import Test.HUnit
import Text.MediaWiki.Wiktionary.Base
import Text.MediaWiki.Wiktionary.English

testExtract :: (Eq a, Show a) => (Text -> a) -> Text -> a -> Test
testExtract func input output = (cs input) ~: (func input) ~?= output

defnTests = [
    testExtract (parseDefinition (termPos "en" "test" "Noun"))
                "# {{senseid|en|first}} definition 1\n# {{senseid|en|second}} definition 2\n# definition 3"
                [WiktionaryFact "definition" (termSense "en" "test" "Noun" "first") (simpleTerm "en" "definition 1"),
                 WiktionaryFact "definition" (termSense "en" "test" "Noun" "second") (simpleTerm "en" "definition 2"),
                 WiktionaryFact "definition" (termSense "en" "test" "Noun" "def.3") (simpleTerm "en" "definition 3")]
    ]

tests = test (defnTests)

main :: IO ()
main = void (runTestTT tests)
