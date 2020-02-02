{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Default.List where

import           Data.Aeson
import           Data.Aeson.Default.Class
import           Data.Kind
import           GHC.Exts                 (IsList (..))
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup
#endif

-- | A 'higher-kined' List.
newtype ListH (t :: (Type -> Type) -> Type) f = ListH { unListH :: [t f] }

instance IsList (ListH t f) where
  type Item (ListH t f) = t f
  fromList = ListH . fromList
  toList (ListH x) = toList x

instance Eq (t f) => Eq (ListH t f) where
  (ListH x) == (ListH y) = x == y

instance Show (t f) => Show (ListH t f) where
  show (ListH x) = show x

instance Read (t f) => Read (ListH t f) where
  readsPrec n s = [(ListH x, s) | (x, s') <- readsPrec n s]

#if MIN_VERSION_base(4, 9, 0)
instance Semigroup (ListH t f) where
  (ListH x) <> (ListH y) = ListH (x <> y)
#endif

instance Monoid (ListH t f) where
  mempty = ListH mempty
  mappend (ListH x) (ListH y) = ListH (mappend x y)

instance FromJSON (t Maybe) => FromJSON (ListH t Maybe) where
  parseJSON = (fmap ListH) . parseJSON

instance (Default t, FromJSON (ListH t Maybe)) => Default (ListH t) where
  constrDef _ = mempty

  applyDef is (ListH []) = is
  applyDef _  (ListH ms) = ListH $ map applyDefs ms

  applyDefs = applyDef $ constrDef "ListH"
