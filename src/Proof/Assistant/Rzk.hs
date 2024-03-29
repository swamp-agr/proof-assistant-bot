{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Rzk where

import Control.Concurrent.Async (race)
import Data.Coerce (coerce)
import System.Mem

import Rzk.Main

import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Response
import Proof.Assistant.Settings
import Proof.Assistant.State

-- | Call Polilyngual API. Rzk will do all the job and return response or an error as result.
callRzk :: InterpreterState InternalInterpreterSettings -> InterpreterRequest -> IO BotResponse
callRzk InterpreterState{..} ir = do
  let InternalInterpreterSettings{..} = settings
      asyncApi = do
        enableAllocationLimit
        setAllocationCounter (fromIntegral allocations)
        let makeResult = pure
              . either toBS toBS
              . typecheckString . fromBS . dropCommand
        makeResult (interpreterRequestMessage ir)
      asyncTimer = asyncWait (coerce timeout)
  eresult <- race asyncTimer (handleErrorMaybe asyncApi)
  case eresult of
    Left () -> pure (TextResponse "Time limit exceeded")
    Right result -> pure (TextResponse result)
