{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App (App (..)) where

import Config.Types
import Gitlab.Client.MTL (HasApiToken (..), HasBaseUrl (..))
import Relude

newtype App a = App {unApp :: ReaderT Config IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Config
    )

instance HasApiToken App where
  getApiToken = App $ asks apiToken

instance HasBaseUrl App where
  getBaseUrl = App $ asks baseUrl
