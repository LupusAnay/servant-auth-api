module Sessions where

import Hasql.Session
import qualified Hasql.Session as Session
import User (UserId, NewUser, User)
import qualified Statements

createUser :: NewUser -> Session UserId
createUser user = Session.statement user Statements.createUser

getAllUsers :: Session [User]
getAllUsers = Session.statement () Statements.getAllUsers