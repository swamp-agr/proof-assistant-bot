{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Rzk where

import Control.Concurrent.Async (race)
import Control.Exception (SomeException (..), catch)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import System.Mem

import Rzk.Polylingual

import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Settings
import Proof.Assistant.State

import qualified Data.ByteString.Char8 as BS8

callRzk :: InterpreterState InternalInterpreterSettings -> InterpreterRequest -> IO ByteString
callRzk InterpreterState{..} ir = do
  let InternalInterpreterSettings{..} = settings
      asyncApi = do
        enableAllocationLimit
        setAllocationCounter (fromIntegral allocations)
        let makeResult = pure
              . either toBS (toBS . compileSomeModule)
              . safeParseSomeModule . fromBS . dropCommand
        makeResult (interpreterRequestMessage ir)
      catchAsyncApi :: SomeException -> IO ByteString
      catchAsyncApi (SomeException ex) = pure (BS8.pack $ show ex)
      asyncTimer = asyncWait (coerce timeout)
  eresult <- race asyncTimer (asyncApi `catch` catchAsyncApi)
  case eresult of
    Left () -> pure "Time limit exceeded"
    Right result -> pure result
