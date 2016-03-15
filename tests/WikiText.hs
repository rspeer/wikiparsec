{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Text.MediaWiki.WikiText
import Text.MediaWiki.AnnotatedText (Annotation, AnnotatedText, makeLink, namespace, page, section)
import qualified Text.MediaWiki.AnnotatedText as A
import Text.MediaWiki.HTML (extractWikiTextFromHTML)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (fromList)
import Data.Either (rights)

testParser :: (Eq a, Show a) => Parser a -> Text -> a -> Test
testParser parser input output =
  (T.unpack input) ~: parseOnly parser input ~?= Right output

testParserFail :: (Eq a, Show a) => Parser a -> Text -> Test
testParserFail parser input =
  (T.unpack input) ~: rights [parseOnly parser input] ~?= []

extractAnnotations :: Either String AnnotatedText -> Either String [Annotation]
extractAnnotations result = case result of
  Left err       -> Left err
  Right annoText -> Right (A.annotations annoText)

testAnnotations :: Parser AnnotatedText -> Text -> [Annotation] -> Test
testAnnotations parser input outputAnnotations =
  (T.unpack input) ~: extractAnnotations (parseOnly parser input) ~?= Right outputAnnotations

linkTests = [
    testParser sectionText "''this'' [[word]]" "this word\n",
    testParser sectionText "[[word|''this'' word]]" "this word\n",
    testParser sectionText "[[word|here's a word]]" "here's a word\n",
    testParser sectionText "this [[word#English]]" "this word\n",
    testParser sectionText "these [[w:en:word]]s" "these words\n",
    testParser sectionText "[[Category:English nouns]]" "English nouns\n",
    testParser sectionText "uphold[ing] the wages system" "uphold[ing] the wages system\n",

    testAnnotations sectionAnnotatedText "this [[word]]" [makeLink {page="word"}],
    testAnnotations sectionAnnotatedText "[[word|''this'' word]]" [makeLink {page="word"}],
    testAnnotations sectionAnnotatedText "this [[word#English]]" [makeLink {page="word", section="English"}],
    testAnnotations sectionAnnotatedText "this [[w:en:word]]" [makeLink {namespace="w:en", page="word"}],
    testAnnotations sectionAnnotatedText "[[Category:English nouns]]" [makeLink {namespace="Category", page="English nouns"}]
    ]

templateTests = [
    testParser template "{{archaic}}" $ fromList
        [("0", "archaic")],
    testParser template "{{t+|fr|exemple|m}}" $ fromList
        [("0", "t+"), ("1", "fr"), ("2", "exemple"), ("3", "m")],
    testParser (specificTemplate "t+") "{{t+|fr|exemple|m}}" $ fromList
        [("0", "t+"), ("1", "fr"), ("2", "exemple"), ("3", "m")],
    testParser template "{{t|ja|例え|tr=[[たとえ]], tatoe}}" $ fromList
        [("0", "t"), ("1", "ja"), ("2", "例え"), ("tr", "たとえ, tatoe")]
    ]

listTests = [
    testParser anyList "* item 1\n* item 2"
        $ BulletList [Item "item 1", Item "item 2"],
    testParser anyList "# item 1\n# item 2\n"
        $ OrderedList [Item "item 1", Item "item 2"],
    testParser anyList "# item 1\n#* item 2a\n#* item 2b\n# item 3"
        $ OrderedList [Item "item 1", BulletList [Item "item 2a", Item "item 2b"], Item "item 3"],
    testParser anyList ":;definition list\n::a syntax that's rarely used for its original purpose"
        $ IndentedList [ListHeading "definition list", IndentedList [Item "a syntax that's rarely used for its original purpose"]],
    testParser anyListText "# item 1\n#; heading\n#* item 2a\n#* item 2b\n# item 3\n"
        $ "item 1\nheading\nitem 2a\nitem 2b\nitem 3\n",
    testParserFail anyListText "",
    testParserFail wikiTextLine ""
    ]

-- Test on a section from Wikipedia's featured article as I wrote this,
-- which was "Symphony No. 8 (Sibelius)".
articleSection = T.unlines [
    "[[File:Ainola yard.jpg|thumb|left|Ainola, Sibelius's home from 1904 until his death|alt=A white house of north European appearance with an orange tiled roof, surrounded by trees]]",
    "Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous [[Grand Duchy of Finland|grand duchy]] within the [[Russian Empire]] having earlier been under Swedish control for many centuries.<ref name= grove3>{{cite web|last= Hepokoski|first= James|title= 1865–89: early years|url= http://www.oxfordmusiconline.com/subscriber/article/grove/music/43725?q=Sibelius&search=quick&pos=1&_start=1|publisher= Grove Music Online|accessdate= 2 August 2013}} {{subscription}}</ref> The country remained divided between a culturally dominant Swedish-speaking minority, to which the Sibelius family belonged, and a more nationalistically-minded Finnish-speaking, or \"[[Fennoman movement|Fennoman]]\" majority.<ref>Rickards, p. 22</ref> In about 1889 Sibelius met his future wife, [[Aino Sibelius|Aino Järnefelt]], who came from a staunch Fennoman family.<ref name= Vesa5>{{cite web|last= Sirén|first= Vesa; Hartikainen, Markku; Kilpeläinen, Kari|title= Studies in Helsinki 1885–1888 |url= http://www.sibelius.fi/english/elamankaari/sib_opinnot_helsinki.htm|publisher= \"Sibelius\" website: Sibelius the Man|accessdate= 2 August 2013|display-authors=etal}}</ref> Sibelius's association with the Järnefelts helped to awaken and develop his own nationalism; in 1892, the year of his marriage to Aino, he completed his first overtly nationalistic work, the symphonic suite ''[[Kullervo (Sibelius)|Kullervo]]''.<ref>Rickards, pp. 50–51</ref> Through the 1890s, as Russian control over the duchy grew increasingly oppressive, Sibelius produced a series of works reflecting Finnish resistance to foreign rule, culminating in the tone poem ''[[Finlandia]]''.<ref>Rickards, pp. 68–69</ref>",
    "{|",
    "| This is a table",
    "| This is still a table",
    "|}",
    "| This shouldn't be a table but we still don't want it"
    ]

articleSectionWikitext = extractWikiTextFromHTML $ articleSection

articleSectionText = T.unlines [
    "Ainola, Sibelius's home from 1904 until his death",
    "Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous grand duchy within the Russian Empire having earlier been under Swedish control for many centuries. The country remained divided between a culturally dominant Swedish-speaking minority, to which the Sibelius family belonged, and a more nationalistically-minded Finnish-speaking, or \"Fennoman\" majority. In about 1889 Sibelius met his future wife, Aino Järnefelt, who came from a staunch Fennoman family. Sibelius's association with the Järnefelts helped to awaken and develop his own nationalism; in 1892, the year of his marriage to Aino, he completed his first overtly nationalistic work, the symphonic suite Kullervo. Through the 1890s, as Russian control over the duchy grew increasingly oppressive, Sibelius produced a series of works reflecting Finnish resistance to foreign rule, culminating in the tone poem Finlandia."
    ]


sectionTests = [
    testParser sectionText articleSectionWikitext articleSectionText
    ]


tableTests = [
   testParser wikiTable "{|\npointless table\n|}" "",
   testParser sectionText "\n|incomplete table\n|}" ""
   ]

miscTests = [
    testParser looseBracket "}" "}",
    testParser sectionText "[[Image:Levellers declaration and standard.gif|thumb|200px|Woodcut from a [[Diggers]] document by [[William Everard (Digger)|William Everard]]]]" "Woodcut from a Diggers document by William Everard\n",
    testParser sectionText "{{template||arg1=1|arg2={{!}}|arg3=}}" "",
    testParser sectionText "{{template|arg1 = {{sub|1}}\n|arg2 = {{sub|3\n}}\n}}\n|template detritus\n|}\nnormal text" "normal text\n"
    ]


tests = test (linkTests ++ templateTests ++ listTests ++ sectionTests ++ tableTests ++ miscTests)

main :: IO ()
main = void (runTestTT tests)
