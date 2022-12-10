{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Rzk where

import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import System.Mem

import Rzk.Polylingual

import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Settings
import Proof.Assistant.State

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
      asyncTimer = asyncWait (coerce timeout)
  eresult <- race asyncTimer (handleErrorMaybe asyncApi)
  case eresult of
    Left () -> pure "Time limit exceeded"
    Right result -> pure result
