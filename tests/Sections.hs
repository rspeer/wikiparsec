{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Text.MediaWiki.Sections
import Text.Parsec.Error
import Control.Monad
import Data.ByteString.UTF8 (toString)
import Data.ByteString (ByteString)
import Data.Map (fromList)
import Data.Either (rights)

testSecParser :: ByteString -> [WikiSection] -> Test
testSecParser input output =
  (toString input) ~: parsePageIntoSections input ~?= Right output

sectionTests = [
    testSecParser "top section\n==Section 1==\ntest\n==Section 2==\nmore\n\n===Section 2.1===\n=Whoops=\ni am not good with computer\n== fake heading" $
       [WikiSection {headings = ["top"], content = "top section\n"},
        WikiSection {headings = ["top","Section 1"], content = "test\n"},
        WikiSection {headings = ["top","Section 2"], content = "more\n\n"},
        WikiSection {headings = ["top","Section 2","Section 2.1"], content = ""},
        WikiSection {headings = ["Whoops"], content = "i am not good with computer\n== fake heading\n"}],
    testSecParser "" [WikiSection {headings = ["top"], content = ""}]
    ]

tests = test (sectionTests)

main :: IO ()
main = void (runTestTT tests)
