> {-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

Useful things to do with association lists.

> module Text.MediaWiki.AList where
> import Data.ByteString (ByteString)
>
> type ByteAssoc = (ByteString, ByteString)
>
> filterEmpty :: [ByteAssoc] -> [ByteAssoc]
> filterEmpty annot = filter (\assoc -> (snd assoc) /= "") annot
>
> get :: (Eq a) => a -> [(a, ByteString)] -> ByteString
> get = getDefault ""
>
> getDefault :: (Eq a) => b -> a -> [(a, b)] -> b
> getDefault def arg alist =
>   case (lookup arg alist) of
>     Just x  -> x
>     Nothing -> def
>
> lookupOne :: (Eq a) => [a] -> [(a, ByteString)] -> Maybe ByteString
> lookupOne [] alist = Nothing
> lookupOne (arg:rest) alist =
>   case (lookup arg alist) of
>     Just "" -> lookupOne rest alist
>     Just x  -> Just x
>     Nothing -> lookupOne rest alist
>
> getOne :: (Eq a) => [a] -> [(a, ByteString)] -> ByteString
> getOne args alist = case (lookupOne args alist) of
>   Just x -> x
>   Nothing -> ""
