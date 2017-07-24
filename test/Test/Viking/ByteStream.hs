{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Viking.ByteStream where

import           Control.Monad.Catch (throwM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import           Hedgehog

import           P

import           System.FilePath ((</>))
import           System.IO (IO, hClose)
import           System.IO.Error (IOError, userError)
import qualified System.IO.Temp as Temp

import qualified Viking.ByteStream as ByteStream

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

tests :: IO Bool
tests =
  checkParallel $$(discover)
