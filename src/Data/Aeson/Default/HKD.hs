{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Default.HKD ( HKD(..) ) where

import           Data.Functor.Identity


{- | Wrap a datatype as __higer-kind data__.

Using the HKD type family means that GHC will automatically erase any Identity
wrappers in our representations.
-}
type family HKD a f where
  HKD a Identity = a
  HKD a f        = f a
