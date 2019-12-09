{-# LANGUAGE NoDeriveAnyClass #-}

module App where

import           Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Reader
import           GHC.Generics
import           Hasql.Pool
import           Hasql.Session
import           Servant              (ServerError)
import           Servant.Server       (Handler)

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)

newtype AppM a =
  AppM
    { runAppM :: ReaderT Pool Handler a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  runSession sess =
    AppM $ do
      pool <- ask
      result <- liftIO $ use pool sess
      runAppM $ pure result
