{-| This library provides a solution for applying a default value to
 'Maybe' fields of 'FromJSON' instances.
You should know a little bit about the __higher-kinded data__(HKD), here is
an [article](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
on this topic.

=== Examples

See "Logging.Config.Type" in [log4hs](http://hackage.haskell.org/package/log4hs)
package for more information on how to use this library in a real project.

>>> :set -XDeriveGeneric
>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XStandaloneDeriving
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           Data.Aeson
>>> import           Data.Aeson.Default
>>> :{
data NameH f = Name { first  :: String
                    , middle :: f String
                    , last_  :: String
                    } deriving Generic
instance FromJSON (NameH Maybe)
instance Default NameH where
  constrDef _ = Name "Jorah" (Identity ".") "Gao"
deriving instance Show (NameH Identity)
data PersonH f = Person { name :: NameH f
                        , age  :: f Int
                        } deriving Generic
instance FromJSON (PersonH Maybe)
instance Default PersonH where
  constrDef _ = Person (constrDef "Name") (Identity 28)
deriving instance Show (PersonH Identity)
:}
>>> decode "{\"first\":\"jorah\", \"last_\": \"gao\"}" :: Maybe (NameH Identity)
Just (Name {first = "jorah", middle = Identity ".", last_ = "gao"})
>>> decode "{\"first\":\"jorah\", \"middle\": \"*\", \"last_\": \"gao\"}" :: Maybe (NameH Identity)
Just (Name {first = "jorah", middle = Identity "*", last_ = "gao"})

>>> :set -XDeriveGeneric
>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XStandaloneDeriving
>>> :set -XTypeFamilies
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           Data.Aeson
>>> import           Data.Aeson.Default
>>> import           Data.Aeson.Default.HKD
>>> :{
data ShapeH f = Square { side :: HKD Double f }
              | Circle { redius :: HKD Double f }
              deriving Generic
instance FromJSON (ShapeH Maybe)
instance Default ShapeH where
  constrDef "Square" = Square 1.0
  constrDef "Circle" = Circle 1.0
deriving instance Show (ShapeH Identity)
data BoxH f = Box { base   :: HKD (ShapeH f) f
                  , height :: HKD Double f
                  } deriving Generic
instance FromJSON (BoxH Maybe)
instance Default BoxH where
  constrDef _ = Box (constrDef "Square") 1.0
deriving instance Show (BoxH Identity)
:}
>>> decode "{}" :: Maybe (BoxH Identity)
Just (Box {base = Square {side = 1.0}, height = 1.0})
>>> decode "{\"base\": {\"tag\": \"Square\"}}" :: Maybe (BoxH Identity)
Just (Box {base = Square {side = 1.0}, height = 1.0})
>>> decode "{\"base\": {\"tag\": \"Circle\"}}" :: Maybe (BoxH Identity)
Just (Box {base = Circle {redius = 1.0}, height = 1.0})
>>> decode "{\"base\": {\"tag\": \"Square\", \"side\": 10.0}}" :: Maybe (BoxH Identity)
Just (Box {base = Square {side = 10.0}, height = 1.0})
>>> decode "{\"height\": 10.0}" :: Maybe (BoxH Identity)
Just (Box {base = Square {side = 1.0}, height = 10.0})
-}
module Data.Aeson.Default
  ( module Data.Aeson.Default.Class
  ) where


import           Data.Aeson.Default.Class
