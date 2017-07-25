{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Viking.ByteStream where

import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as Builder

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.FilePath ((</>))
import           System.IO (IO, hClose)
import           System.IO.Error (IOError, userError)
import qualified System.IO.Temp as Temp

import qualified Viking.ByteStream as ByteStream
import qualified Viking.Stream as Stream

import           X.Control.Monad.Trans.Either (runEitherT)


prop_get_contents_exception :: Property
prop_get_contents_exception =
  withTests 1 . property . test . runResourceT $ do
    (_, _, h) <- Temp.openBinaryTempFile Nothing "viking-"
    liftIO $ hClose h

    x <- runEitherT . ByteStream.effects $ ByteStream.hGetContents h
    annotateShow x

    assert $
      isLeft x

prop_read_file_exception :: Property
prop_read_file_exception =
  withTests 1 . property . test . runResourceT $ do
    (_, dir) <- Temp.createTempDirectory Nothing "viking-"

    x <- runEitherT . ByteStream.effects $ ByteStream.readFile (dir </> "foo")
    annotateShow x

    assert $
      isLeft x

prop_write_file_exception :: Property
prop_write_file_exception =
  withTests 1 . property . test . runResourceT $ do
    (_, dir) <- Temp.createTempDirectory Nothing "viking-"

    let
     err =
       userError "prop_write_file_exception"

    x :: Either IOError () <-
      runEitherT $ ByteStream.writeFile (dir </> "foo") (throwM err)

    x === Left err

prop_builders :: Property
prop_builders =
  property $ do
    bss0 <- forAll . Gen.list (Range.linear 0 10) $ Gen.bytes (Range.linear 0 100)

    bs <-
      ByteStream.toStrict_ .
        ByteStream.fromBuilders .
        Stream.each $
        fmap Builder.byteString bss0

    Strict.concat bss0 === bs

tests :: IO Bool
tests =
  checkParallel $$(discover)
