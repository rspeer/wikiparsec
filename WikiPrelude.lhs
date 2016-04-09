> {-# LANGUAGE NoImplicitPrelude #-}
> module WikiPrelude (module ClassyPrelude, replace) where
> import ClassyPrelude
> import qualified Data.Text as T
> 
> replace = T.replace
