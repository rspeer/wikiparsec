{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Test.HUnit
import Text.Language.Normalize
import WikiPrelude
import Control.Monad

testNorm :: Language -> Text -> Text -> Test
testNorm lang input output = (unpack input) ~: (normalizeText lang input) ~?= output

normTests = [
    testNorm "sl" "življénje" "življenje",
    testNorm "ru" "осты́нуть" "остынуть",
    testNorm "ru" "горой" "горой",
    testNorm "en" "naïve" "naïve",
    testNorm "es" "cañon" "cañon",
    testNorm "de" "schön" "schön",
    testNorm "de" "Fußball" "Fußball"
    ]

tests = test (normTests)

main :: IO ()
main = void (runTestTT tests)
