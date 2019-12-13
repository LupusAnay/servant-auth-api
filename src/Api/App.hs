{-# LANGUAGE NoDeriveAnyClass #-}

module Api.App where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.AppSettings
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Labels (Field)
import GHC.Generics (Generic)
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (Session)
import Servant (Handler, ServerError)
import Servant.Server

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)

class Monad m =>
      MonadSettings m
  where
  getSetting :: (AppSettings -> a) -> m a

data Env =
  Env
    { dbPool :: Pool
    , appSettings :: AppSettings
    }

newtype AppM a =
  AppM
    { runAppM :: ReaderT Env Handler a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  runSession sess =
    AppM $ do
      env <- ask
      let pool = dbPool env
      result <- liftIO $ use pool sess
      runAppM $ pure result

instance MonadSettings AppM where
  getSetting f =
    AppM $ do
      env <- ask
      let settings = appSettings env
      pure $ f settings

class ToServerError a where
  convert :: a -> ServerError

liftEither' :: (ToServerError a, MonadError ServerError m) => Either a b -> m b
liftEither' (Right x) = pure x
liftEither' (Left x) = throwError $ convert x

liftMaybe' :: (MonadError ServerError m) => Maybe a -> m a
liftMaybe' (Just x) = pure x
liftMaybe' Nothing = throwError err404

instance ToServerError UsageError where
  convert err = err500 {errBody = LBS.fromStrict $ BS.pack $ show err}