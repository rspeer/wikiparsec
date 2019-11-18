{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import WikiPrelude
import Test.HUnit
import Text.MediaWiki.WikiText
import Text.MediaWiki.AnnotatedText (Annotation, AnnotatedText, makeLink, getAnnotations)
import Text.MediaWiki.Templates (ignoreTemplates, idTemplate, useArg, multiTemplate)
import Text.MediaWiki.HTML (extractWikiTextFromHTML)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Monad

testParser :: (Eq a, Show a) => Parser a -> Text -> a -> Test
testParser parser input output =
  (cs input) ~: parseOnly parser input ~?= Right output



bvTests = [
    testParser (wikiTextLine (const multiTemplate))
        "speech as described in {{bibleverse||John|19:13|}}"
        "speech as described in John 19:13",
    testParser (wikiTextLine (const multiTemplate))
        "Israelites East of the Jordan into captivity {{bibleverse|2|Kings|15:28|NIV}};"
        "Israelites East of the Jordan into captivity 2 Kings 15:28;"  
    ]

langTests = [
    testParser (wikiTextLine (const multiTemplate)) 
        "ceci est un {{lang|de|german word}}" 
        "ceci est un german word",
    testParser (wikiTextLine (const multiTemplate)) 
        "a computer which had been developed by a team around {{lang|de|[[Niklaus Wirth]]}}" 
        "a computer which had been developed by a team around Niklaus Wirth",
    testParser (wikiTextLine (const multiTemplate))
        "Once the bill was formally enacted, Victoria began signing her letters Victoria R ({{lang-la|Regina et Imperatrix}}, that is, Queen and Empress)" 
        "Once the bill was formally enacted, Victoria began signing her letters Victoria R (Regina et Imperatrix, that is, Queen and Empress)"
    ]

asofTests = [
    testParser (wikiTextLine (const multiTemplate)) 
        "ceci est un {{as of|2010|lc=y}}" 
        "ceci est un as of 2010",
    testParser (wikiTextLine (const multiTemplate)) 
        "{{As of|2019|lc=yes||df=|post=,}} the Beatles hold the record for [[List of artists by number of Billboard Hot 100 number-one singles|most number-one hits on the Hot 100]] chart with twenty." 
        "As of 2019, the Beatles hold the record for most number-one hits on the Hot 100 chart with twenty.",
    testParser (wikiTextLine (const multiTemplate)) 
        "{{As of|2011|3|df=US}}, non-supervisory hourly wages ranged from $11.00 to $21.00 in the U.S." 
        "As of March 2011, non-supervisory hourly wages ranged from $11.00 to $21.00 in the U.S.",
    testParser (wikiTextLine (const multiTemplate)) 
        "{{as of|2015|July|31|lc=y|df=US}}" 
        "as of July 31st 2015",
    testParser (wikiTextLine (const multiTemplate)) 
        "Walmart Argentina was founded in 1995 and, {{As of|2019|7|31|df=US|lc=y|post=,}} operates 92 stores under the banners Walmart Supercenter" 
        "Walmart Argentina was founded in 1995 and, As of July 31st 2019, operates 92 stores under the banners Walmart Supercenter"
    ]

convertTests = [
    testParser (wikiTextLine (const multiTemplate)) 
        "ceci est un {{convert|3|km|mi}}" 
        "ceci est un 3km",
    testParser (wikiTextLine (const multiTemplate)) 
        "A standard [[Ball (association football)|association football (soccer) ball]] (with a diameter of {{cvt|22|cm|in|disp=or}}) subtends an angle of 1 arcminute at a distance of approximately {{cvt|775|m|yd}}" 
        "A standard association football (soccer) ball (with a diameter of 22cm) subtends an angle of 1 arcminute at a distance of approximately 775m",
    testParser (wikiTextLine (const multiTemplate)) 
        "{{cvt|5|–|10|mm|in|frac=8}}" 
        "5–10mm",
    testParser (wikiTextLine (const multiTemplate)) 
        "A slightly undulating plain extends from the seacoast about {{convert|10|or|12|mi|km|spell=in|abbr=off}} inland," 
        "A slightly undulating plain extends from the seacoast about 10 or 12mi inland,",
    testParser (wikiTextLine (const multiTemplate)) 
        "There are fourteen (eight on 787-9) private cabins on most of these aircraft, each with a {{cvt|6|ft|6|in|m}} bed, a {{convert|15|in|cm|adj=on}} wide entertainment screen," 
        "There are fourteen (eight on 787-9) private cabins on most of these aircraft, each with a 6ft6in bed, a 15in wide entertainment screen,",
    testParser (wikiTextLine (const multiTemplate)) 
        "Beginning in about 2500 BC, the ancient Egyptians began to produce their own blue pigment known as [[Egyptian blue]] by grinding [[silica]], [[Lime (material)|lime]], [[copper]], and [[alkalai]], and heating it to {{convert|800|or| 900|C}}." 
        "Beginning in about 2500 BC, the ancient Egyptians began to produce their own blue pigment known as Egyptian blue by grinding silica, lime, copper, and alkalai, and heating it to 800 or 900C."
    ]

tests = test (asofTests ++ convertTests ++ langTests ++ bvTests)

main :: IO ()
main = void (runTestTT tests)
