import Test.HUnit
import Text.Wiki.MediaWiki
import Text.Parsec.Error
import Control.Monad
import Data.Map (fromList)
import Data.Either (rights)

testParser parser input output =
  input ~: parse parser "(test)" input ~?= Right output

testParserFail parser input =
  input ~: rights [parse parser "(test)" input] ~?= []

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
    testParser (specificTemplate "t+") "{{t+|fr|exemple|m}}" $ fromList
        [("0", "t+"), ("1", "fr"), ("2", "exemple"), ("3", "m")],
    testParser template "{{t|ja|例え|tr=[[たとえ]], tatoe}}" $ fromList
        [("0", "t"), ("1", "ja"), ("2", "例え"), ("tr", "たとえ, tatoe")],
    testLinks "{{t|ja|例え|tr=[[たとえ]], tatoe}}" [makeLink {page="たとえ"}]
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
        $ "item 1\nheading\nitem 2a\nitem 2b\n\nitem 3\n",
    testParserFail anyListText "",
    testParserFail wikiTextLine ""
    ]

miscTests = [
    testParser looseBracket "}" "}",
    testParser wikiText "<math>A =\\left ( \\frac{1329\\times10^{-H/5}}{D} \\right ) ^2</math>" "",
    testParser wikiText "[[Image:Levellers declaration and standard.gif|thumb|200px|Woodcut from a [[Diggers]] document by [[William Everard (Digger)|William Everard]]]]" "",
    testParser wikiText "{{template||arg1=1|arg2={{!}}|arg3=}}" ""
    ]

-- Test on a section from Wikipedia's featured article as I wrote this,
-- which was "Symphony No. 8 (Sibelius)".
articleSection = unlines [
    "==Background==",
    "[[File:Ainola yard.jpg|thumb|left|Ainola, Sibelius's home from 1904 until his death|alt=A white house of north European appearance with an orange tiled roof, surrounded by trees]]",
    "Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous [[Grand Duchy of Finland|grand duchy]] within the [[Russian Empire]] having earlier been under Swedish control for many centuries.<ref name= grove3>{{cite web|last= Hepokoski|first= James|title= 1865–89: early years|url= http://www.oxfordmusiconline.com/subscriber/article/grove/music/43725?q=Sibelius&search=quick&pos=1&_start=1|publisher= Grove Music Online|accessdate= 2 August 2013}} {{subscription}}</ref> The country remained divided between a culturally dominant Swedish-speaking minority, to which the Sibelius family belonged, and a more nationalistically-minded Finnish-speaking, or \"[[Fennoman movement|Fennoman]]\" majority.<ref>Rickards, p. 22</ref> In about 1889 Sibelius met his future wife, [[Aino Sibelius|Aino Järnefelt]], who came from a staunch Fennoman family.<ref name= Vesa5>{{cite web|last= Sirén|first= Vesa; Hartikainen, Markku; Kilpeläinen, Kari|title= Studies in Helsinki 1885–1888 |url= http://www.sibelius.fi/english/elamankaari/sib_opinnot_helsinki.htm|publisher= \"Sibelius\" website: Sibelius the Man|accessdate= 2 August 2013|display-authors=etal}}</ref> Sibelius's association with the Järnefelts helped to awaken and develop his own nationalism; in 1892, the year of his marriage to Aino, he completed his first overtly nationalistic work, the symphonic suite ''[[Kullervo (Sibelius)|Kullervo]]''.<ref>Rickards, pp. 50–51</ref> Through the 1890s, as Russian control over the duchy grew increasingly oppressive, Sibelius produced a series of works reflecting Finnish resistance to foreign rule, culminating in the tone poem ''[[Finlandia]]''.<ref>Rickards, pp. 68–69</ref>",
    "",
    "===Subsection===",
    "This subsection wasn't actually in the article.",
    "{|",
    "| This is a table",
    "| This is still a table",
    "|}"
    ]

articleSectionText = unlines [
    "Background",
    "",
    "",
    "Jean Sibelius was born in 1865 in Finland, since 1809 an autonomous grand duchy within the Russian Empire having earlier been under Swedish control for many centuries. The country remained divided between a culturally dominant Swedish-speaking minority, to which the Sibelius family belonged, and a more nationalistically-minded Finnish-speaking, or \"Fennoman\" majority. In about 1889 Sibelius met his future wife, Aino Järnefelt, who came from a staunch Fennoman family. Sibelius's association with the Järnefelts helped to awaken and develop his own nationalism; in 1892, the year of his marriage to Aino, he completed his first overtly nationalistic work, the symphonic suite Kullervo. Through the 1890s, as Russian control over the duchy grew increasingly oppressive, Sibelius produced a series of works reflecting Finnish resistance to foreign rule, culminating in the tone poem Finlandia.",
    "",
    "Subsection",
    "",
    "This subsection wasn't actually in the article.",
    "",
    "",
    ""
    ]


sectionTests = [
    testParser (heading 2) "== Heading ==\n" "Heading",
    testParser (heading 3) "=== Heading ===\n" "Heading",
    testParserFail (heading 2) "=== Heading ===\n",
    testParserFail (heading 3) "== Heading ==\n",
    testParserFail (sectionContent 2) articleSection,
    testParser (sectionText 2) articleSection articleSectionText
    ]


tableTests = [
   testParser wikiTable "{|\npointless table\n|}" "pointless table",
   testParser wikiTable "{|\n|incomplete table" "|incomplete table",
   testParser wikiTable "\n|incomplete table\n|}" ""
   ]


tests = test (linkTests ++ templateTests ++ listTests ++ sectionTests ++ tableTests ++ miscTests)

main :: IO ()
main = void (runTestTT tests)
