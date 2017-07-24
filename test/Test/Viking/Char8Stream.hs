{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Viking.Char8Stream where

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8
import           Data.Functor.Identity (runIdentity)
import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)

import           Viking
import qualified Viking.ByteStream as ByteStream
import qualified Viking.Char8Stream as Char8Stream
import qualified Viking.Stream as Stream


prop_rechunk_line_end :: Property
prop_rechunk_line_end =
  withTests 1000 . property $ do
    bss0 <- forAll . Gen.list (Range.linear 0 10) $ Gen.bytes (Range.linear 1 100)

    let
      bss :> () =
        runIdentity .
        Stream.toList .
        ByteStream.toChunks .
        Char8Stream.rechunkLineEnd .
        ByteStream.fromChunks $
        Stream.each bss0

    annotateShow bss

    assert $
      all (('\n' ==) . Char8.last) (List.drop 1 $ List.reverse bss)

    Strict.concat bss0 === Strict.concat bss

tests :: IO Bool
tests =
  checkParallel $$(discover)
