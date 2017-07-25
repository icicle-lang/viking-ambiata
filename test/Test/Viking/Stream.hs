{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Viking.Stream where

import           Data.Functor.Identity (runIdentity)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           P

import           System.IO (IO)

import qualified Viking.Stream as Stream


prop_when_empty :: Property
prop_when_empty =
  property $ do
    x <- forAll Gen.alpha
    xs0 <- forAll $ Gen.list (Range.linear 0 5) Gen.alpha

    let
      xs =
        runIdentity .
        Stream.toList_ .
        Stream.whenEmpty x $
        Stream.each xs0

    annotateShow xs

    assert $
      not (null xs)

tests :: IO Bool
tests =
  checkParallel $$(discover)
