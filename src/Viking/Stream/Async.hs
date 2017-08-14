{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Viking.Stream.Async (
    mapConcurrently
  , mapConcurrentlyN
  ) where

import           Control.Concurrent (getNumCapabilities)
import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Monad.Base (liftBase)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           P

import           System.IO (IO)

import           Viking
import qualified Viking.Stream as Stream


mapConcurrentlyN :: MonadBaseControl IO m => Int -> (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapConcurrentlyN n f =
  Stream.concat .
  Stream.mapM (Async.mapConcurrently f) .
  Stream.mapped Stream.toList .
  Stream.chunksOf n
{-# INLINABLE mapConcurrentlyN #-}

mapConcurrently :: MonadBaseControl IO m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapConcurrently f xs = do
  n <- liftBase getNumCapabilities
  mapConcurrentlyN n f xs
{-# INLINABLE mapConcurrently #-}
