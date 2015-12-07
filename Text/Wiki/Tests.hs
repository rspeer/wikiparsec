import Test.HUnit
import Text.Wiki.MediaWiki
import Text.Parsec.Error

-- I don't know why we need to re-define this here.
-- I just copied this out of the Text.Parsec.Error source.
instance Eq ParseError where
    l == r
        = errorPos l == errorPos r && messageStrs l == messageStrs r
        where
          messageStrs = map messageString . errorMessages

testParser parser input output =
  parse parser "(test)" input ~?= Right output

testLinks = testParser wikiTextLinks

linkTests = test [
    testLinks "this [[word]]" [makeLink {page="word"}],
    testLinks "[[word|this word]]" [makeLink {page="word"}],
    testLinks "this [[word#English]]" [makeLink {page="word", section="English"}],
    testLinks "this [[w:en:word]]" [makeLink {namespace="w:en", page="word"}],
    testLinks "[[Category:English nouns]]" [makeLink {namespace="Category", page="English nouns"}]
    ]

tests = linkTests
main = runTestTT tests
