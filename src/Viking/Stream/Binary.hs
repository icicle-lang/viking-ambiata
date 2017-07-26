{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Viking.Stream.Binary (
    runGet
  , runGetSome
  , runGetMany

  , BinaryError(..)
  , renderBinaryError
  ) where

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)

import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as Strict
import qualified Data.Text as Text

import           P

import           X.Control.Monad.Trans.Either (EitherT, left)

import           Viking
import qualified Viking.ByteStream as ByteStream
import qualified Viking.Stream as Stream


data BinaryError =
    BinaryDecodeError !Text
    deriving (Eq, Show)

renderBinaryError :: BinaryError -> Text
renderBinaryError = \case
  BinaryDecodeError msg ->
    "Binary decode error: " <> msg

-- | Run a 'Get' binary decoder over a 'ByteStream'.
--
--   Return decoded value, as well as the rest of the stream.
--
runGet :: Monad m => Get a -> ByteStream m r -> EitherT BinaryError m (a, ByteStream m r)
runGet get input =
  let
    loop bss0 = \case
      Get.Fail _bs _off err ->
        left . BinaryDecodeError $ Text.pack err

      Get.Done bs _off x ->
        pure (x, ByteStream.consChunk bs bss0)

      Get.Partial k -> do
        e <- lift $ ByteStream.nextChunk bss0
        case e of
          Left r ->
            loop (pure r) (k Nothing)

          Right (bs, bss) ->
            loop bss (k (Just bs))
  in
    loop input (Get.runGetIncremental get)

-- | Run a 'Get' binary decoder over a 'ByteStream' one or more times.
--
runGetSome :: Monad m => Get a -> ByteStream m r -> Stream (Of a) (EitherT BinaryError m) r
runGetSome get input =
  let
    nextGet bss0 = do
      e <- lift $ ByteStream.nextChunk bss0
      case e of
        Left r ->
          pure r
        Right (bs, bss) ->
          loop bss (Get.runGetIncremental get `Get.pushChunk` bs)

    loop bss0 = \case
      Get.Fail _bs _off err ->
        lift . left . BinaryDecodeError $ Text.pack err

      Get.Done bs _off x -> do
        Stream.yield x
        if Strict.null bs then
          nextGet bss0
        else
          loop bss0 (Get.runGetIncremental get `Get.pushChunk` bs)

      Get.Partial k -> do
        e <- lift $ ByteStream.nextChunk bss0
        case e of
          Left r ->
            loop (pure r) (k Nothing)

          Right (bs, bss) ->
            loop bss (k (Just bs))
  in
    loop (hoist lift input) (Get.runGetIncremental get)

-- | Run a 'Get' binary decoder over a 'ByteStream' zero or more times.
--
runGetMany :: Monad m => Get a -> ByteStream m r -> Stream (Of a) (EitherT BinaryError m) r
runGetMany get input = do
  e <- lift . lift $ ByteStream.nextChunk input
  case e of
    Left r ->
      pure r
    Right (hd, tl) ->
      if Strict.null hd then
        runGetMany get tl
      else
        runGetSome get (ByteStream.consChunk hd tl)
