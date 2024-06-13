{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Util (timerToIO, writerToIO) where

import Data.Time (getCurrentTime)
import Effects (Timer (..), Writer (..))
import Polysemy
import Relude

timerToIO :: (Member (Embed IO) r) => Sem (Timer ': r) a -> Sem r a
timerToIO = interpret $ \case
  GetCurrentTime -> embed getCurrentTime

writerToIO :: (Member (Embed IO) r) => Sem (Writer ': r) a -> Sem r a
writerToIO = interpret $ \case
  Write msg -> embed $ putTextLn @IO msg
