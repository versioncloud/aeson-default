{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main ( main ) where


import           Control.Exception           (assert)
import           Data.Aeson
import           Data.Aeson.Default
import           Data.Aeson.Default.HKD
import           Data.Aeson.Default.List
import           Data.Aeson.Default.Map.Lazy
import           Data.Functor.Identity
import           Data.Map.Lazy
import           Data.Maybe
import           GHC.Generics


data NameH f = Name { first  :: String
                    , middle :: f String
                    , last_  :: String
                    } deriving Generic

instance FromJSON (NameH Maybe)

instance Default NameH where
  constrDef _ = Name "Jorah" (Identity ".") "Gao"

deriving instance Eq (NameH Identity)


data PersonH f = Person { name :: NameH f
                        , age  :: f Int
                        } deriving Generic

instance FromJSON (PersonH Maybe)

instance Default PersonH where
  constrDef _ = Person (constrDef "Name") (Identity 28)

deriving instance Eq (PersonH Identity)


data DBH f = DB { host :: HKD String f
                , port :: HKD Int f
                } deriving Generic

instance FromJSON (DBH Maybe)

instance Default DBH where
  constrDef _ = DB "localhost" 3306

deriving instance Eq (DBH Identity)


data ConfigH f = Config { db :: HKD (DBH f) f } deriving Generic

instance FromJSON (ConfigH Maybe)

instance Default ConfigH where
  constrDef _ = Config (constrDef "DB")

deriving instance Eq (ConfigH Identity)


data ShapeH f = Square { side :: HKD Double f }
              | Circle { redius :: HKD Double f }
              deriving Generic

instance FromJSON (ShapeH Maybe)

instance Default ShapeH where
  constrDef "Square" = Square 1.0
  constrDef "Circle" = Circle 1.0

deriving instance Eq (ShapeH Identity)


data BoxH f = Box { base   :: HKD (ShapeH f) f
                  , height :: HKD Double f
                  } deriving Generic

instance FromJSON (BoxH Maybe)

instance Default BoxH where
  constrDef _ = Box (constrDef "Square") 1.0

deriving instance Eq (BoxH Identity)


data HandlerH f = Handler { level    :: HKD String f
                          , filterer :: HKD [String] f
                          } deriving Generic

instance FromJSON (HandlerH Maybe)

instance Default HandlerH where
  constrDef _ = Handler "INFO" []

deriving instance Eq (HandlerH Identity)


data SinkH f = Sink { handlers :: HKD (ListH HandlerH f) f } deriving Generic

instance FromJSON (SinkH Maybe)

instance Default SinkH where
  constrDef _ = Sink (constrDef "ListH")

deriving instance Eq (SinkH Identity)


data ManagerH f = Manager { sinks :: HKD (MapH String SinkH f) f
                          } deriving Generic

instance FromJSON (ManagerH Maybe)

instance Default ManagerH where
  constrDef _ = Manager $ constrDef "MapH"

deriving instance Eq (ManagerH Identity)


it :: String -> Bool -> IO ()
it = flip assert . putStrLn

main :: IO ()
main = do
  putStrLn "\n<<<<<<<<<<<<<<<<<<<<<< Test <<<<<<<<<<<<<<<<<<<<<<"

  it "flatten data" $
    let res1 = decode "{\"first\":\"Jorah\", \"last_\": \"Gao\"}" == Just (Name "Jorah" (Identity ".") "Gao")
        res2 = decode "{\"first\":\"Jorah1\", \"last_\": \"Gao2\"}" == Just (Name "Jorah1" (Identity ".") "Gao2")
        res3 = decode "{\"first\":\"Jorah3\", \"middle\": \"*\", \"last_\": \"Gao4\"}" == Just (Name "Jorah3" (Identity "*") "Gao4")
        res4 = decode "{}" == Just (DB "localhost" 3306 :: DBH Identity)
        res5 = decode "{\"host\": \"127.0.0.1\"}" == Just (DB "127.0.0.1" 3306 :: DBH Identity)
        res6 = decode "{\"port\": 3307}" == Just (DB "localhost" 3307 :: DBH Identity)
        res7 = decode "{\"host\": \"127.0.0.1\", \"port\": 3307}" == Just (DB "127.0.0.1" 3307 :: DBH Identity)
    in all id [res1, res2, res3, res4, res5, res6, res7]
  it "nested data" $
    let res1 = decode "{\"name\": {\"first\": \"Jorah5\", \"last_\": \"Gao6\"}}" == Just (Person (Name "Jorah5" (Identity ".") "Gao6") (Identity 28))
        res2 = decode "{\"name\": {\"first\": \"Jorah5\", \"last_\": \"Gao6\"}, \"age\": 82}" == Just (Person (Name "Jorah5" (Identity ".") "Gao6") (Identity 82))
        res3 = decode "{}" == Just (Config (DB "localhost" 3306) :: ConfigH Identity)
        res4 = decode "{\"db\": {}}" == Just (Config (DB "localhost" 3306) :: ConfigH Identity)
        res5 = decode "{\"db\": {\"host\": \"127.0.0.1\"}}" == Just (Config (DB "127.0.0.1" 3306) :: ConfigH Identity)
        res6 = decode "{\"db\": {\"port\": 3307}}" == Just (Config (DB "localhost" 3307) :: ConfigH Identity)
        res7 = decode "{\"db\": {\"host\": \"127.0.0.1\", \"port\": 3307}}" == Just (Config (DB "127.0.0.1" 3307) :: ConfigH Identity)
    in all id [res1, res2, res3, res4, res5, res6, res7]
  it "multiple data constructors" $
    let res1 = decode "{}" == Just (Box (Square 1.0) 1.0 :: BoxH Identity)
        res2 = decode "{\"base\": {\"tag\": \"Square\"}}" == Just (Box (Square 1.0) 1.0 :: BoxH Identity)
        res3 = decode "{\"base\": {\"tag\": \"Circle\"}}" == Just (Box (Circle 1.0) 1.0 :: BoxH Identity)
        res4 = decode "{\"base\": {\"tag\": \"Square\", \"side\": 10.0}}" == Just (Box (Square 10.0) 1.0 :: BoxH Identity)
        res5 = decode "{\"base\": {\"tag\": \"Circle\", \"redius\": 10.0}}" == Just (Box (Circle 10.0) 1.0 :: BoxH Identity)
        res6 = decode "{\"height\": 10.0}" == Just (Box (Square 1.0) 10.0 :: BoxH Identity)
     in all id [res1, res2, res3, res4, res5, res6]
  it "higher-kined container types" $
    let res1 = decode "{}" == Just (Manager mempty :: ManagerH Identity)
        res2 = decode "{\"sinks\": {}}" == Just (Manager mempty :: ManagerH Identity)
        res3 = decode "{\"sinks\": {\"root\": {}}}" == Just (Manager (MapH $ fromList [("root", Sink mempty)]) :: ManagerH Identity)
        res4 = decode "{\"sinks\": {\"root\": {}, \"root1\": {}}}" == Just (Manager (MapH $ fromList [("root", Sink mempty), ("root1", Sink mempty)]) :: ManagerH Identity)
        res5 = decode "{\"sinks\": {\"root\": {\"handlers\": []}}}" == Just (Manager (MapH $ fromList [("root", Sink mempty)]) :: ManagerH Identity)
        res6 = decode "{\"sinks\": {\"root\": {\"handlers\": [{}]}}}" == Just (Manager (MapH $ fromList [("root", Sink $ ListH [Handler "INFO" []])]) :: ManagerH Identity)
        res7 = decode "{\"sinks\": {\"root\": {\"handlers\": [{\"level\": \"DEBUG\"}]}}}" == Just (Manager (MapH $ fromList [("root", Sink $ ListH [Handler "DEBUG" []])]) :: ManagerH Identity)
        res8 = decode "{\"sinks\": {\"root\": {\"handlers\": [{\"filterer\": []}]}}}" == Just (Manager (MapH $ fromList [("root", Sink $ ListH [Handler "INFO" []])]) :: ManagerH Identity)
        res9 = decode "{\"sinks\": {\"root\": {\"handlers\": [{\"filterer\": [\"A\"]}]}}}" == Just (Manager (MapH $ fromList [("root", Sink $ ListH [Handler "INFO" ["A"]])]) :: ManagerH Identity)
        res10 = decode "{\"sinks\": {\"root\": {\"handlers\": [{\"level\": \"DEBUG\", \"filterer\": [\"A\"]}]}}}" == Just (Manager (MapH $ fromList [("root", Sink $ ListH [Handler "DEBUG" ["A"]])]) :: ManagerH Identity)
     in all id [res1, res2, res3, res4, res5, res6, res7, res8, res9, res10]

  putStrLn "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
