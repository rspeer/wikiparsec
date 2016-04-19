{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Test.HUnit
import WikiPrelude hiding (assert)
import Text.MediaWiki.AnnotatedText
import Text.MediaWiki.Wiktionary.Base
import Data.Aeson (encode, decode, Value)

linkTest = makeLink "" "test" ""
linkExample = makeLink "" "example" "#English"
linkOffWiki = makeLink "w" "example" ""
senseID = mapFromList [("senseID", "challenge, trial")]
senseName = "challenge, trial" :: Text
at1 = annotate [linkTest, senseID] "test"
at2 = annotate [linkExample] "example"

english = toLanguage "en"
german = toLanguage "de"
russian = toLanguage "ru"
termEnglish = simpleTerm english "example"
termGerman  = simpleTerm german "Beispiel"
-- termRussian gets automatically normalized to "остынуть"
termRussian = simpleTerm russian "осты́нуть"
fact1 = makeFact "translation" termRussian termEnglish

factJSON :: Text
factJSON = "{\"rel\":\"translation\",\"to\":{\"text\":\"example\",\"language\":\"en\"},\"from\":{\"text\":\"остынуть\",\"language\":\"ru\"}}"
factJSONValue = (decode (cs factJSON)) :: Maybe Value
redecoded = (decode (encode fact1)) :: Maybe Value

-- define a third AnnotatedText as a string literal
atLiteral :: AnnotatedText
atLiteral = "literal"

annoWriter = buildA $ do
  put "1" "one"
  put "2" "two"
  return "test"

annoWriter2 = buildA $ do
  return "literal"

tests = test [
    "show term" ~: (show termRussian) ~?= "(term [\"остынуть\",\"ru\"])",
    "replace relation" ~: assignRel "distrusts" fact1 ~?= WiktionaryFact "distrusts" termRussian termEnglish,
    "JSON decodes properly" ~: assert (isJust redecoded),
    "JSON fact matches" ~: decode (encode fact1) ~?= factJSONValue,
    "find sense ID" ~: findSenseIDInList (getAnnotations at1) ~?= (Just senseName),

    "annotation is linkable" ~: assert (linkableAnnotation linkTest),
    "off-wiki annotation is not linkable" ~: assert (not (linkableAnnotation linkOffWiki)),
    "annotation is plain link" ~: assert (plainLinkAnnotation linkTest),
    "linkable annotations" ~: linkableAnnotations at1 ~?= [linkTest],
    "plain link annotations" ~: plainLinkAnnotations at1 ~?= [linkTest],

    "fact from annotation" ~: fact1 ~?= assignRel "translation" (annotationToFact english termRussian linkExample),

    "split def 1" ~: splitDefinition "this is one definition; this is another." ~?= ["this is one definition", "this is another"],
    "split def 2" ~: splitDefinition "this is one definition, with a comma" ~?= ["this is one definition, with a comma"],
    "split def 3" ~: splitDefinition "definition, punctuated" ~?= ["definition", "punctuated"],

    "find prefixed heading 1" ~: findPrefixedHeading "Etymology" ["top", "English"] ~?= Nothing,
    "find prefixed heading 2" ~: findPrefixedHeading "Etymology" ["top", "English", "Etymology"] ~?= Just "",
    "find prefixed heading 3" ~: findPrefixedHeading "Etymology" ["top", "English", "Etymology 1", "Noun"] ~?= Just " 1",
    "annotated text from writer" ~: annoWriter ~?= annotate [annotationFromList [("1","one"),("2","two")]] "test"
    ]

main :: IO ()
main = void (runTestTT tests)
