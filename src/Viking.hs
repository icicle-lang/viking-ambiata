{-# LANGUAGE NoImplicitPrelude #-}
module Viking (
  -- $overview

  -- * General
  -- $general
    Stream
  , Of(..)

  -- * Specialized
  , ByteStream
  ) where

import           Data.ByteString.Streaming (ByteString)

import           Streaming (Stream, Of(..))

-- $overview
--
-- This library is intended to be used with the one or more of the following
-- imports:
--
-- @
-- import           Viking (Of(..), Stream, ByteStream)
-- import qualified Viking.Stream as Stream
-- import qualified Viking.ByteStream as ByteStream
-- import qualified Viking.Char8Stream as Char8Stream
-- @
--

-- $general
--
-- The 'Stream' data type can represent any effectful succession of steps,
-- where the form of the steps or 'commands' is specified by the first
-- parameter.
--
-- @
-- data Stream f m r =
--     Step !(f (Stream f m r))
--   | Effect (m (Stream f m r))
--   | Return r
-- @
--
-- In the common case, effectful linked-lists, the first parameter is
-- instantiated as @Of a@ where 'Of' is a left-strict tuple.
--
-- @
-- data Of a b =
--   !a :> b
--
-- data Stream (Of a) m r =
--     Step !(Of a (Stream (Of a) m r))
--   | Effect (m (Stream (Of a) m r))
--   | Return r
-- @
--

-- | This is a type synonym for streaming
--   'Data.ByteString.Streaming.ByteString' to make it possible to use in
--   conjunction with strict 'Data.ByteString.ByteString' without having to
--   qualify both types.
--
--   This can be used with the "Viking.ByteStream" interface, or with the
--   "Viking.Char8Stream" interface.
--
-- @
-- data ByteStream m r =
--     Empty r
--   | Chunk !Strict.ByteString (ByteStream m r)
--   | Go (m (ByteStream m r))
-- @
--
type ByteStream =
  ByteString
