module Lib
  ( startApp
  , app
  ) where

import Data.Generics.Internal.VL.Lens
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import User

type Auth = String

type CrudAPI c r u d = 
       ReqBody '[ JSON] c :> Post '[ JSON] Int
  :<|> Capture "id" Int :> Get '[ JSON] r
  :<|> Get '[ JSON] [r]
  :<|> ReqBody '[ JSON] u :> Capture "id" Int :> Patch '[ JSON] Int
  :<|> Capture "id" Int :> Delete '[ JSON] d

type UsersAPI = "users" :> CrudAPI NewUser User NewUser ()

server :: Server UsersAPI
server = createUser
    :<|> getUser
    :<|> getUsers
    :<|> patchUser
    :<|> deleteUser

createUser :: NewUser -> Handler UserId
createUser user = pure (-1)

getUsers :: Handler [User]
getUsers = do
  user <- getUser 1
  pure [user]

getUser :: UserId -> Handler User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: NewUser -> UserId -> Handler UserId
patchUser user id = pure 1

deleteUser :: UserId -> Handler ()
deleteUser id = pure ()

api :: Proxy UsersAPI
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app
