{-# LANGUAGE NoDeriveAnyClass #-}

module Api.App where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import GHC.Generics (Generic)
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (Session)
import Servant (Handler, ServerError)
import Servant.Server (err500)

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

class ToServerError a where
  convert :: a -> ServerError

liftEither' :: (ToServerError a, MonadError ServerError m) => Either a b -> m b
liftEither' (Right x) = pure x
liftEither' (Left x) = throwError $ convert x

liftMaybe' :: (MonadError ServerError m) => Maybe a -> m a
liftMaybe' (Just x) = pure x
liftMaybe' Nothing = throwError err500

instance ToServerError UsageError where
  convert err = err500