module Data.User where

import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics
import           Servant.Auth.Server

type UserId = Int

-- TODO: Skip password serialization in ToJSON
data User =
  User
    { userId   :: UserId
    , username :: String
    , email    :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data AuthData =
  AuthData
    { username :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data NewUser =
  NewUser
    { username :: String
    , email    :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
