import Test.HUnit
import Text.Wiki.MediaWiki
import Text.Parsec.Error
import Data.Map (fromList)

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

linkTests = [
    testParser wikiText "this [[word]]" "this word",
    testParser wikiText "[[word|this word]]" "this word",
    testParser wikiText "this [[word#English]]" "this word",
    testParser wikiText "these [[w:en:word]]s" "these words",
    testParser wikiText "[[Category:English nouns]]" "",

    testLinks "this [[word]]" [makeLink {page="word"}],
    testLinks "[[word|this word]]" [makeLink {page="word"}],
    testLinks "this [[word#English]]" [makeLink {page="word", section="English"}],
    testLinks "this [[w:en:word]]" [makeLink {namespace="w:en", page="word"}],
    testLinks "[[Category:English nouns]]" [makeLink {namespace="Category", page="English nouns"}]
    ]

templateTests = [
    testParser template "{{archaic}}" $ fromList
        [("0", "archaic")],
    testParser template "{{t+|fr|exemple|m}}" $ fromList
        [("0", "t+"), ("1", "fr"), ("2", "exemple"), ("3", "m")],
    testParser template "{{t|ja|例え|tr=[[たとえ]], tatoe}}" $ fromList
        [("0", "t"), ("1", "ja"), ("2", "例え"), ("tr", "たとえ, tatoe")],
    testLinks "{{t|ja|例え|tr=[[たとえ]], tatoe}}" [makeLink {page="たとえ"}]
    ]

tests = test (linkTests ++ templateTests)
main = runTestTT tests
