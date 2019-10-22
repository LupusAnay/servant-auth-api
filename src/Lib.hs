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

type UsersAPI = "users" :> ReqBody '[ JSON] NewUser :> Post '[ JSON] UserId 
           :<|> "users" :> Capture "id" UserId :> Get '[ JSON] User
           :<|> "users" :> Get '[ JSON] [User]
           :<|> "users" :> ReqBody '[ JSON] NewUser :> Capture "id" UserId :> Patch '[ JSON] UserId 
           :<|> "users" :> Capture "id" UserId :> Delete '[ JSON] ()

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy UsersAPI
api = Proxy

server :: Server UsersAPI
server = createUser 
    :<|> getUser 
    :<|> getUsers
    :<|> patchUser 
    :<|> deleteUser

createUser :: NewUser -> Handler UserId
createUser user = pure (-1)

getUsers :: Handler [User]
getUsers = pure []

getUser :: UserId -> Handler User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: NewUser -> UserId -> Handler UserId
patchUser user id = pure 1

deleteUser :: UserId -> Handler ()
deleteUser id = pure ()