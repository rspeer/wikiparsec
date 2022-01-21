{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Text.SplitUtils
import Control.Monad

splitTests = [
    -- Test the case where the delimiter doesn't appear
    splitFirst ":" "test" ~?= ("test", ""),
    splitLast ":" "test" ~?= ("", "test"),

    -- Cases where the delimiter appears once should be the same
    -- for splitFirst and splitLast, except for the delimiter
    splitFirst ":" ":test" ~?= ("", ":test"),
    splitLast ":" ":test" ~?= (":", "test"),
    splitFirst ":" "test:" ~?= ("test", ":"),
    splitLast ":" "test:" ~?= ("test:", ""),
    splitFirst ":" "test:one" ~?= ("test", ":one"),
    splitLast ":" "test:one" ~?= ("test:", "one"),

    -- They differ again when there are multiple delimiters
    splitFirst ":" "test:one:two" ~?= ("test", ":one:two"),
    splitLast ":" "test:one:two" ~?= ("test:one:", "two"),

    -- Unicode is okay
    splitFirst "," "tëst,øne,tẃo" ~?= ("tëst", ",øne,tẃo"),
    splitLast "," "tëst,øne,tẃo" ~?= ("tëst,øne,", "tẃo"),
    splitFirst "☃" "tëst☃øne☃tẃo" ~?= ("tëst", "☃øne☃tẃo"),
    splitLast "☃" "tëst☃øne☃tẃo" ~?= ("tëst☃øne☃", "tẃo")]

balanceTests = [
    removeParentheticals "text" ~?= "text",
    removeParentheticals "text (clarification)" ~?= "text",
    removeParentheticals "text (clarification (too much)) and more" ~?= "text and more",
    removeParentheticals "ASCII ()" ~?= "ASCII"
    removeParentheticals "(212) 555-1212" ~?= " 555-1212",
    removeParentheticals "((what" ~?= "",
    removeParentheticals "))what" ~?= "what",
    removeParentheticals ")(" ~?= "",
    removeParentheticals "(())" ~?= ""]

tests = test (splitTests ++ balanceTests)
main = void (runTestTT tests)
