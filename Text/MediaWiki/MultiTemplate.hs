{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Text.MediaWiki.MultiTemplate where
import WikiPrelude
import Text.MediaWiki.AnnotatedText
import qualified Data.Text as T
import qualified ClassyPrelude as P
import qualified Data.List as L
import qualified Data.Char as C

import Prelude (reads)


multiSwitch :: [(Text, Text)] -> Text
multiSwitch t | lnme == "as of"                   = asOfHandler (t, name)
              | lnme == "asof"                    = asOfHandler (t, name)

              | lnme == "convert"                 = convertHandler t
              | lnme == "cvt"                     = convertHandler t

              | lnme == "lang"                    = langHandler t
              | T.isPrefixOf "lang-" lnme         = langdHandler t
              | T.isPrefixOf "interlanguage" lnme = get "1" t
              | T.isPrefixOf "vr" lnme            = get "1" t

              | T.isPrefixOf "bibleverse" lnme    = bibleHandler t

              | lnme == "sic"                     = name
              | lnme == "ipa"                     = get "1" t

              | otherwise         = ""
              where name = get "0" t
                    lnme = T.toLower $ name

--Lang
langHandler :: [(Text, Text)] -> Text
langHandler t = (get "2" t)
langdHandler :: [(Text, Text)] -> Text
langdHandler t = (get "1" t)

-- Bibleverse
bibleHandler :: [(Text, Text)] -> Text
bibleHandler t = L.foldl' bibleFold "" $ (bibleGetList t)

bibleGetList :: [(Text, Text)] -> [Text]
bibleGetList t  | le == "" = l
                | C.isAlpha $ T.head $ le = L.init l
                | otherwise = l
                where l = L.drop 1 $ getElems t
                      le = L.last l

bibleFold :: Text -> Text -> Text
bibleFold ret elem | elem == "" = ret
                   | T.length ret == 0 = elem
                   | otherwise = ret ++ " " ++ elem
-- convert stuff
convertHandler :: [(Text, Text)] -> Text
convertHandler t = (get "1" t) ++ (convertHandleSpacing $ (get "2" t)) ++ (convertEndHandler t)

convertHandleSpacing :: Text -> Text
convertHandleSpacing t | t == "and" = " and "
                       | t == "or"  = " or "
                       | t == "to"  = " to "
                       | otherwise = t

convertEndHandler :: [(Text, Text)] -> Text
convertEndHandler t = res
                    where (_,_,res) = (L.foldl' convertFold (False, False, "") $ convertGetTrimList t)

convertGetTrimList :: [(Text, Text)] -> [Text]
convertGetTrimList t | L.length l == 0 = l
                     | isNumeric $ T.unpack $ (L.last l) = L.init l
                     | otherwise = l
                     where l = L.drop 3 (getElems t)

convertFold :: (Bool, Bool, Text) -> Text -> (Bool, Bool, Text)
convertFold (prev_num, fin, txt) e | prev_num && (not fin) = (False, False, txt ++ (convertHandleSpacing elem))
                                   | (isNumeric $ T.unpack elem) && (not fin) = (True, False, txt ++ elem)
                                   | (isNumeric $ T.unpack elem) && (not fin) = (True, False, txt)
                                   | otherwise                 = (False, True, txt)
                                    where elem = T.strip e


isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
    
isDouble s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

justElems :: ([Text], [Text]) -> [Text]
justElems (t1, t2) = t2

getElems :: [(Text, Text)] -> [Text]
getElems t = justElems $ L.unzip t

-- asof stuff
asOfHandler :: ([(Text, Text)], Text) -> Text
asOfHandler (t, name) = name ++ " " ++ dateHandler t ++ (get "post" t)

dateHandler :: [(Text, Text)] -> Text
dateHandler t | not ((get "3" t) == "") = fmt_all (get "1" t) (get "2" t) (get "3" t)
              | not ((get "2" t) == "") = fmt_2 (get "1" t) (get "2" t)
              | otherwise = (get "1" t)

fmt_all :: Text -> Text -> Text -> Text
fmt_all y m d = (fmt_mon m) ++ (fmt_d d) ++ y

fmt_2 :: Text -> Text -> Text
fmt_2 y m = (fmt_mon m) ++ y

fmt_mon :: Text -> Text
fmt_mon m | m == "1"  = "January "
          | m == "2"  = "February "
          | m == "3"  = "March "
          | m == "4"  = "April "
          | m == "5"  = "May "
          | m == "6"  = "June "
          | m == "7"  = "July "
          | m == "8"  = "August "
          | m == "9"  = "September "
          | m == "10" = "October "
          | m == "11" = "November "
          | m == "12" = "December "
          | otherwise = m ++ " "

fmt_d :: Text -> Text
fmt_d d | d == "1"   = "1st "
        | d == "2"   = "2nd "
        | d == "3"   = "3rd "
        | d == "21"  = "21st "
        | d == "22"  = "22nd "
        | d == "23"  = "23rd "
        | d == "31"  = "31st "
        | otherwise = d ++ "th "