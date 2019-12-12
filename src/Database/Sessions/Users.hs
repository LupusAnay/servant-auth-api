module Database.Sessions.Users where

import           Data.User
import qualified Database.Statements.Users as Statements
import           Hasql.Session
import qualified Hasql.Session             as Session

createUser :: NewUser -> Session UserId
createUser user = Session.statement user Statements.createUser

getAllUsers :: Session [User]
getAllUsers = Session.statement () Statements.getAllUsers

deleteUser :: UserId -> Session ()
deleteUser id = Session.statement id Statements.deleteUser

updateUser :: UserId -> NewUser -> Session ()
updateUser id user = Session.statement (id, user) Statements.updateUser
