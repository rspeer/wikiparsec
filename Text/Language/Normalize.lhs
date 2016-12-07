`Text.Language.Normalize` - converts words to a standard form
=============================================================

> {-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
> module Text.Language.Normalize where
> import WikiPrelude
> import Data.Text.ICU.Normalize (normalize, NormalizationMode(..))

Wiktionary has some automatic translations that it applies in language-tagged
links. You can write a link in some fully-explicit dictionary form
with accent marks, vowel points, or whatever, and Wiktionary will reduce
it so that it links to a more common entry name without the extra marks.

Different transformations are applied to different languages, but in this module
we've generalized them into two categories: languages that remove accents from
letters, and languages that leave accents alone.

MediaWiki's implementation is part of its "Module:languages" Lua code. The
configuration for all languages with two-letter language codes can be seen
at: https://en.wiktionary.org/wiki/Module:languages/data2.

We apply a sequence of steps to the text:

- Replace U+671 ARABIC LETTER ALEF WASLA with U+617 ARABIC LETTER ALEF

- Decompose the characters into NFD form, so that diacritics and marks appear
  as their own codepoints

- Remove marks that should always be removed: those that annotate the
  pronunciation of a word in Arabic, Hebrew, or Syriac, plus a couple of
  miscellaneous marks used in dictionary-ese

- If this language is in `diacriticDroppingLanguages`, remove all codepoints
  from U+300 to U+36F, the Unicode block for diacritics that combine with
  Latin, Cyrillic, and Greek letters, except that we leave U+30C COMBINING
  CARON. See "Diacritics and when to drop them" for why.

- Recompose the characters into NFC form.

> normalizeText :: Language -> Text -> Text
> normalizeText lang =
>   if (elem lang diacriticDroppingLanguages)
>     then (normalize NFC) . filterDiacritics . filterMarks . (normalize NFD) . replaceAlifWasla
>     else (normalize NFC) . filterMarks . (normalize NFD) . replaceAlifWasla


Diacritics and when to drop them
--------------------------------

In some languages, accents or diacritics on top of characters are very
important, as they change one letter to another. The letter `ä` is
distinct from `a` in German and many Scandinavian languages, for example,
and Spanish `ñ` is distinct from `n`.

In other languages, words are only written with accents in dictionaries,
so to get the word that people actually use, you need to strip the accents
off.

There are some languages that want to remove all diacritics *except* for
carons, because the caron makes a lexically-distinct letter, while other
diacritics are just accent marks used in dictionary annotations. For example,
if the text "življénje" is linked in Slovene (language "sl"), it will link to
"življenje", because `ž` is different from `z` but `é` is just a marked-up
version of `e`.

There don't seem to be any languages that use carons as dictionary markings
that should be removed, so we simply always leave carons as is.

> diacriticDroppingLanguages :: [Language]
> diacriticDroppingLanguages = [
>   "ab", "be", "bg", "ce", "cu", "el", "ka", "la", "lt",
>   "mk", "ny", "os", "ru", "sh", "sl", "so", "tg", "uk",
>   "ang", "arc", "chl", "dum", "enm", "gmh", "gml", "goh", "got", "grc",
>   "hil", "huz", "inh", "kjj", "lui", "lzz", "mga", "miq", "moe",
>   "nci", "odt", "ofs", "oge", "olt", "osx", "ppl", "rue", "sga", "sva",
>   "unm", "xfa", "xmk"]
>
> filterDiacritics :: Text -> Text
> filterDiacritics = (filter (not . isDiacritic)) . fixCyrillicVowels
>
> isDiacritic :: Char -> Bool
> isDiacritic c =
>   let ord = fromEnum c in
>     -- skip 0x30c (caron)
>     (0x300 <= ord && ord <= 0x30b) ||
>     (0x30d <= ord && ord <= 0x36f)

Some accents are used for markup on Latin letters, but are an essential part of
a Cyrillic letter. For example, `ĕ` should be the same as `e`, particularly in
Latin-language entries. But `Й` should not become `И`, even though the
combining character (U+306 COMBINING BREVE) is the same.

To prevent Cyrillic vowels from being modified by this procedure, we
re-compose them first.

> fixCyrillicVowels =
>   (replace "Й" "Й") .
>   (replace "й" "й") .
>   (replace "Ё" "Ё") .
>   (replace "ё" "ё")

We define "marks" to be notation that should always be removed, particularly
those used in dictionary entries of Semitic languages.

> isMark :: Char -> Bool
> isMark c =
>   let ord = fromEnum c in
>     -- Arabic combining marks
>     (0x64b <= ord && ord <= 0x65f) ||
>     -- Arabic tatweel and superscript alif
>     ord == 0x640 || ord == 0x670 ||
>     -- Hebrew marks
>     (0x591 <= ord && ord <= 0x5bd) ||
>     (0x5bf <= ord && ord <= 0x5c5) ||
>     ord == 0x5c7 ||
>     -- Syriac marks
>     (0x730 <= ord && ord <= 0x748) ||
>     -- Cyrillic kamora
>     ord == 0x484 ||
>     -- middle-dot used for annotating many dictionary entries
>     c == '·'
>
> filterMarks :: Text -> Text
> filterMarks = filter (not . isMark)
>
> replaceAlifWasla :: Text -> Text
> replaceAlifWasla = replace "ٱ" "ا"
>

Things we haven't dealt with:

- Marks in Armenian, which seem to mostly be punctuation but there's an
  "emphasis mark" that may affect entry names
- Northern Sami apostrophes between double consonants
- Thai hyphens
- Kurdish apostrophe normalization

