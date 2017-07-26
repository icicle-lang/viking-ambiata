{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Viking.ByteStream (
    module Data.ByteString.Streaming

  , consChunk

  , embed

  , hoistEither
  , runEitherT
  , injectEitherT

  , defaultChunkSize

  , readFile
  , readFileN
  , writeFile

  , hGetContents
  , hGetContentsN
  , hPut

  , fromBuilders
  ) where

import           Control.Monad.Catch (MonadCatch, try, bracket)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import           Control.Monad.Trans.Resource (MonadResource)

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Streaming as Streaming
import           Data.ByteString.Streaming hiding (ByteString, readFile, writeFile, hGetContents, hGetContentsN, hPut)
import qualified Data.ByteString.Streaming.Internal as Streaming
import           Data.ByteString.Streaming.Internal (consChunk)

import           P

import           System.IO (IO, FilePath, Handle, IOMode(..))
import qualified System.IO as IO
import           System.IO.Error (IOError)

import           Viking
import qualified Viking.Stream as Stream

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)
import qualified X.Control.Monad.Trans.Either as EitherT


embed :: Monad n => (forall a. m a -> ByteStream n a) -> ByteStream m b -> ByteStream n b
embed phi =
  let
    loop = \case
      Streaming.Empty r ->
        Streaming.Empty r
      Streaming.Chunk bs rest ->
        Streaming.Chunk bs (loop rest)
      Streaming.Go m ->
        loop =<< phi m
  in
    loop
{-# INLINABLE embed #-}

hoistEither :: Monad m => ByteStream m (Either x r) -> ByteStream (EitherT x m) r
hoistEither =
  bind (lift . EitherT.hoistEither) . hoist lift
{-# INLINABLE hoistEither #-}

runEitherT :: Monad m => ByteStream (EitherT x m) r -> ByteStream m (Either x r)
runEitherT =
  EitherT.runEitherT . distribute
{-# INLINABLE runEitherT #-}

injectEitherT :: Monad m => EitherT x (ByteStream m) r -> ByteStream (EitherT x m) r
injectEitherT =
  hoistEither . EitherT.runEitherT
{-# INLINABLE injectEitherT #-}

defaultChunkSize :: Int
defaultChunkSize =
  1024 * 1024
{-# INLINABLE defaultChunkSize #-}

readFileN :: (MonadResource m, MonadCatch m) => Int -> FilePath -> ByteStream (EitherT IOError m) ()
readFileN chunkSize path =
  embed (lift . EitherT . try) .
    Streaming.bracketByteString (IO.openBinaryFile path ReadMode) IO.hClose $
      Streaming.hGetContentsN chunkSize
{-# INLINABLE readFileN #-}

readFile :: (MonadResource m, MonadCatch m) => FilePath -> ByteStream (EitherT IOError m) ()
readFile =
  readFileN defaultChunkSize
{-# INLINABLE readFile #-}

writeFile :: (MonadBaseControl IO m, MonadIO m, MonadCatch m) => FilePath -> ByteStream m r -> EitherT IOError m r
writeFile path bss =
  EitherT . try $
    liftBaseOp
      (bracket (IO.openBinaryFile path WriteMode) (IO.hClose))
      (\h -> Streaming.hPut h bss)
{-# INLINABLE writeFile #-}

hGetContentsN :: (MonadIO m, MonadCatch m) => Int -> Handle -> ByteStream (EitherT IOError m) ()
hGetContentsN chunkSize handle =
  embed (lift . EitherT . try) $
    Streaming.hGetContentsN chunkSize handle
{-# INLINABLE hGetContentsN #-}

hGetContents :: (MonadIO m, MonadCatch m) => Handle -> ByteStream (EitherT IOError m) ()
hGetContents =
  hGetContentsN defaultChunkSize
{-# INLINABLE hGetContents #-}

hPut :: (MonadIO m, MonadCatch m) => Handle -> ByteStream m r -> EitherT IOError m r
hPut handle bss =
  EitherT . try $
    Streaming.hPut handle bss
{-# INLINABLE hPut #-}

fromBuilders :: Monad m => Stream (Of Builder) m r -> ByteStream m r
fromBuilders =
  let
    fromElement (x :> r) = do
      Streaming.fromLazy (Builder.toLazyByteString x)
      pure r
  in
    Streaming.concat . Stream.maps fromElement
{-# INLINABLE fromBuilders #-}
