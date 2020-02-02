{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Aeson.Default.Class
  ( Default(..)
  ) where


import           Data.Aeson
import           Data.Functor.Identity
import           Data.Kind
import           Data.Maybe
import           GHC.Generics


data Mismatch = Mismatch


{-| In most cases, use the default implementation for 'Generic' instances.

Since 'Default' instances have implemented 'FromJSON' (t 'Maybe'),
all 'Default' instances will automatically implement 'FromJSON' (t 'Identity').
-}
class FromJSON (t Maybe) => Default (t :: (Type -> Type) -> Type) where
  -- | Get default value by the data constructor name.
  constrDef :: String -> t Identity

  {-| Apply the given default value, if the data constructor does not match,
  call 'constrDef' to get the correct value and then apply it again, if it does
  not match either, raise an error.

  There is a default implementation for 'Generic' instances.
  -}
  applyDef :: t Identity -> t Maybe -> t Identity

  default applyDef :: ( Generic (t Identity)
                      , Generic (t Maybe)
                      , GDefault (Rep (t Identity)) (Rep (t Maybe))
                      , GConsName (Rep (t Identity))
                      , GConsName (Rep (t Maybe))
                      )
                   => t Identity -> t Maybe -> t Identity
  applyDef i m | Right r <- gapplyDef (from i) (from m) = to r
  applyDef _ m = retry (constrDef $ gconsName $ from m) m
    where
      retry :: ( Generic (t Identity)
               , Generic (t Maybe)
               , GDefault (Rep (t Identity)) (Rep (t Maybe))
               , GConsName (Rep (t Identity))
               , GConsName (Rep (t Maybe))
               )
            => t Identity -> t Maybe -> t Identity
      retry i m | Right r <- gapplyDef (from i) (from m) = to r
      retry i m = error $
        "Data.Aeson.Default: The data constructor (" ++ (gconsName (from i)) ++
        ") of the default value you provide (or constrDef returns) " ++
        " does not match expected (" ++ (gconsName (from m)) ++ ")."

  applyDefs :: t Maybe -> t Identity

  -- | Call 'constrDef' to get the default value, then call 'applyDef' to apply
  -- it.
  default applyDefs :: ( Generic (t Maybe)
                       , GConsName (Rep (t Maybe))
                       )
                    => t Maybe -> t Identity
  applyDefs m = applyDef (constrDef $ gconsName $ from m) m

instance Default t => FromJSON (t Identity) where
  parseJSON = (fmap applyDefs) . parseJSON


--------------------------------------------------------------------------------
class GDefault f g where
  gapplyDef :: f (t Identity) -> g (t Maybe) -> Either Mismatch (f (t Identity))

-- Data type
instance GDefault f g => GDefault (D1 c f) (D1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Choice between data constructors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :+: f') (g :+: g') where
  gapplyDef (L1 p) (L1 k) = L1 <$> gapplyDef p k
  gapplyDef (R1 p) (R1 k) = R1 <$> gapplyDef p k
  gapplyDef _ _           = Left Mismatch

-- Data constructor
instance ( Constructor c
         , GDefault f g
         ) => GDefault (C1 c f) (C1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Enum type (nullary data constructor)
instance Constructor c => GDefault (C1 c U1) (C1 c U1) where
  gapplyDef (M1 p) (M1 k) = Right $ M1 p

-- Apply record selectors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :*: f') (g :*: g') where
  gapplyDef (p :*: p') (k :*: k') = do
    x <- gapplyDef p k
    y <- gapplyDef p' k'
    return $ x :*: y

-- Selector
instance (Selector c , GDefault f g) => GDefault (S1 c f) (S1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Not nested required field
instance GDefault (K1 i f) (K1 i f) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 k

-- Not nested optional field (use type family)
instance GDefault (K1 i f) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 $ fromMaybe p k

-- Not nested optional field (not use type family)
instance GDefault (K1 i (Identity f)) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 Nothing)  = Right $ K1 p
  gapplyDef (K1 p) (K1 (Just k)) = Right $ K1 $ Identity k

-- Nested required field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (t Maybe)) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 $ applyDef p k

-- Nested optional field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (Maybe (t Maybe))) where
  gapplyDef (K1 p) (K1 Nothing)  = Right $ K1 p
  gapplyDef (K1 p) (K1 (Just k)) = Right $ K1 $ applyDef p k


class GConsName f where
  gconsName :: f p -> String

instance GConsName f => GConsName (D1 c f) where
  gconsName (M1 x) = gconsName x

instance (GConsName f, GConsName g)=> GConsName (f :+: g) where
  gconsName (L1 x) = gconsName x
  gconsName (R1 x) = gconsName x

instance Constructor c => GConsName (C1 c f) where
  gconsName = conName
