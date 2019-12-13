module Data.AppSettings where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

data AppSettings =
  AppSettings
    { dbHost      :: String
    , dbPort      :: Int
    , dbUser      :: String
    , dbName      :: String
    , dbPassword  :: String
    , dbPoolSize  :: Int
    , secretKey   :: String
    , serverPort  :: Int
    , jwtLifeTime :: Int
    }
  deriving (Show, Generic, FromJSON, ToJSON)

defaultAppSettings :: AppSettings
defaultAppSettings =
  AppSettings
    { dbHost = "localhost"
    , dbPort = 5432
    , dbUser = "postgres_t"
    , dbName = "postgres"
    , dbPassword = "postrgres"
    , dbPoolSize = 2
    , secretKey = "jsBvv0frrmPIaqoII34U472mXhXBtPJzh9pReDc9mJA="
    , serverPort = 8080
    , jwtLifeTime = 604800
    }
