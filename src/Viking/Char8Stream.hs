{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Viking.Char8Stream (
    module Data.ByteString.Streaming.Char8

  , rechunkLineEnd
  ) where

import qualified Data.ByteString as Strict
import           Data.ByteString.Streaming.Char8 hiding (ByteString)
import qualified Data.ByteString.Streaming.Internal as Streaming

import           P

import           Viking (ByteStream)


-- | Ensures that every chunk in the stream ends on a @'\n'@ line feed character.
--
rechunkLineEnd :: Monad m => ByteStream m r -> ByteStream m r
rechunkLineEnd incoming =
  let
    loop dl = \case
      Streaming.Empty r ->
        let
          !bs =
            Strict.concat $! dl []
        in
          if Strict.null bs then
            Streaming.Empty r
          else
            Streaming.Chunk bs (Streaming.Empty r)

      Streaming.Chunk bs0 bss ->
        case Strict.elemIndexEnd 0x0A bs0 of
          Nothing ->
            loop (dl . (bs0 :)) bss
          Just ix ->
            let
              (!bs1, !bs2) =
                Strict.splitAt (ix + 1) bs0
            in
              Streaming.Chunk (Strict.concat $! dl [bs1]) (loop (bs2 :) bss)

      Streaming.Go m ->
        Streaming.Go (fmap (loop dl) m)
  in
    loop id incoming
{-# INLINABLE rechunkLineEnd #-}
