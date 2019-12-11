{-# LANGUAGE NoDeriveAnyClass #-}

module Api.App where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, ask)
import           GHC.Generics           (Generic)
import           Hasql.Pool             (Pool, UsageError, use)
import           Hasql.Session          (Session)
import           Servant                (Handler, ServerError)

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
