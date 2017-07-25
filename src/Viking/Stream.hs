{-# LANGUAGE NoImplicitPrelude #-}
module Viking.Stream (
    module Streaming.Prelude

  , whenEmpty
  ) where

import           Control.Monad.Trans.Class (lift)

import           P

import           Streaming.Prelude


-- | Returns either the stream, or a new singleton stream containing the value
--   if the stream turns out to be empty.
--
whenEmpty :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
whenEmpty def input = do
  e <- lift $ next input
  case e of
    Left r -> do
      yield def
      pure r

    Right (hd, tl) ->
      cons hd tl
{-# INLINABLE whenEmpty #-}
