module Data.User where

import           Data.Aeson
import           GHC.Generics
import           Servant.Auth.Server

type UserId = Int

data User =
  User
    { userId   :: UserId
    , username :: String
    , email    :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON)

instance ToJSON User where
  toJSON (User id name email _) = object ["userId" .= id, "username" .= name, "email" .= email]

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
