{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Default.Map.Strict where

import           Data.Aeson
import           Data.Aeson.Default.Class
import           Data.Kind
import           Data.Map.Strict          hiding (fromList, toList)
import           GHC.Exts                 (IsList (..))
import           GHC.Generics
import           Prelude                  hiding (lookup, map)
#if MIN_VERSION_base(4, 9, 0)
import           Data.Semigroup
#endif


-- | A 'higer-kinded' Strict Map.
newtype MapH k (t :: (Type -> Type) -> Type) f = MapH { unMapH :: Map k (t f) }

instance Ord k => IsList (MapH k t f) where
  type Item (MapH k t f) = (k,t f)
  fromList = MapH . fromList
  toList (MapH x) = toList x

instance (Eq k, Eq (t f)) => Eq (MapH k t f) where
 (MapH x) == (MapH y) = x == y

instance (Show k, Show (t f)) => Show (MapH k t f) where
  show (MapH x) = show x

instance (Ord k, Ord (t f)) => Ord (MapH k t f) where
  compare (MapH x) (MapH y) = compare x y

instance (Ord k, Read k, Read (t f)) => Read (MapH k t f) where
  readsPrec n s = [(MapH x, s') | (x, s') <- readsPrec n s]

#if MIN_VERSION_base(4, 9, 0)
instance Ord k => Semigroup (MapH k t f) where
  (MapH x) <> (MapH y) = MapH (x <> y)
#endif

instance Ord k => Monoid (MapH k t f) where
  mempty = MapH mempty
  mappend (MapH x) (MapH y) = MapH (mappend x y)

instance (Ord k, FromJSONKey k, FromJSON (t Maybe)
         ) => FromJSON (MapH k t Maybe) where
  parseJSON = (fmap MapH) . parseJSON

instance (Ord k, Default t, FromJSON (MapH k t Maybe)
         ) => Default (MapH k t) where
  constrDef _ = mempty

  applyDef (MapH is) (MapH ms) = MapH $ mapWithKey applyItem ms
    where
      applyItem k m | Just i <- lookup k is = applyDef i m
      applyItem _ m = applyDefs m

  applyDefs = applyDef $ constrDef "MapH"
