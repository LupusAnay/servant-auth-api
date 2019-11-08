module Database where

import Database.PostgreSQL.Simple (Connection)
import Data.Pool
import Control.Monad.Reader
import Servant.Server (Handler)

class (Monad m) => MonadDB m where
  withConnection :: (Connection -> m a) -> m a

newtype H a = H { runH :: ReaderT (Pool Connection) Handler a }
  deriving (Functor, Applicative, Monad)

instance MonadDB H where
  withConnection f = H $ do
    pool <- ask
    withResource pool $ \conn -> runH (f conn)