import Test.HUnit
import Text.Wiki.MediaWiki
import Text.Parsec.Error
import Data.Map (fromList)

testParser parser input output =
  parse parser "(test)" input ~?= Right output

testLinks = testParser wikiTextLinks

linkTests = [
    testParser wikiText "''this'' [[word]]" "this word",
    testParser wikiText "[[word|''this'' word]]" "this word",
    testParser wikiText "[[word|here's a word]]" "here's a word",
    testParser wikiText "this [[word#English]]" "this word",
    testParser wikiText "these [[w:en:word]]s" "these words",
    testParser wikiText "[[Category:English nouns]]" "",

    testLinks "this [[word]]" [makeLink {page="word"}],
    testLinks "[[word|''this'' word]]" [makeLink {page="word"}],
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

listTests = [
    testParser anyList "* item 1\n* item 2" $ BulletList [Item "item 1", Item "item 2"],
    testParser anyList "# item 1\n# item 2\n" $ OrderedList [Item "item 1", Item "item 2"],
    testParser anyList "# item 1\n#* item 2a\n#* item 2b\n# item 3" $ OrderedList [Item "item 1", BulletList [Item "item 2a", Item "item 2b"], Item "item 3"]
    ]

tests = test (linkTests ++ templateTests ++ listTests)
main = runTestTT tests
