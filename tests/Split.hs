import Test.HUnit
import Text.Wiki.SplitUtils
import Control.Monad

splitTests = test [
    -- Test the case where the delimiter doesn't appear
    splitFirst ':' "test" ~?= ("test", ""),
    splitLast ':' "test" ~?= ("", "test"),

    -- Cases where the delimiter appears once should be the same
    -- for splitFirst and splitLast
    splitFirst ':' ":test" ~?= ("", "test"),
    splitLast ':' ":test" ~?= ("", "test"),
    splitFirst ':' "test:" ~?= ("test", ""),
    splitLast ':' "test:" ~?= ("test", ""),
    splitFirst ':' "test:one" ~?= ("test", "one"),
    splitLast ':' "test:one" ~?= ("test", "one"),

    -- They differ again when there are multiple delimiters
    splitFirst ':' "test:one:two" ~?= ("test", "one:two"),
    splitLast ':' "test:one:two" ~?= ("test:one", "two"),

    -- Unicode is okay
    splitFirst ',' "tëst,øne,tẃo" ~?= ("tëst", "øne,tẃo"),
    splitLast ',' "tëst,øne,tẃo" ~?= ("tëst,øne", "tẃo"),
    splitFirst '☃' "tëst☃øne☃tẃo" ~?= ("tëst", "øne☃tẃo"),
    splitLast '☃' "tëst☃øne☃tẃo" ~?= ("tëst☃øne", "tẃo")]

tests = splitTests
main = void (runTestTT tests)
