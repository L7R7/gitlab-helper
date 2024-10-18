{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App (App (..)) where

import Config.Types
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Gitlab.Client.MTL (HasApiToken (..), HasBaseUrl (..), HasUserAgent (..))
import Relude

newtype App a = App {unApp :: ReaderT Config IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Config,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadUnliftIO
    )

instance HasApiToken App where
  getApiToken = App $ asks apiToken

instance HasBaseUrl App where
  getBaseUrl = App $ asks baseUrl

instance HasUserAgent App where
  getUserAgent = App $ asks userAgent
