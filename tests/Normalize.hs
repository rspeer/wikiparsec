{-# LANGUAGE OverloadedStrings #-}

-- TODO: clean up unnecessary imports
import Test.HUnit
import Text.Language.Normalize
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8

testNorm :: ByteString -> ByteString -> ByteString -> Test
testNorm lang input output = (UTF8.toString input) ~: (normalizeBytes lang input) ~?= output

normTests = [
    testNorm "sl" (UTF8.fromString "življénje") (UTF8.fromString "življenje"),
    testNorm "ru" (UTF8.fromString "осты́нуть") (UTF8.fromString "остынуть"),
    testNorm "ru" (UTF8.fromString "горой") (UTF8.fromString "горой"),
    testNorm "en" (UTF8.fromString "naïve") (UTF8.fromString "naïve"),
    testNorm "es" (UTF8.fromString "cañon") (UTF8.fromString "cañon"),
    testNorm "de" (UTF8.fromString "schön") (UTF8.fromString "schön"),
    testNorm "de" (UTF8.fromString "Fußball") (UTF8.fromString "Fußball")
    ]

tests = test (normTests)

main :: IO ()
main = void (runTestTT tests)
