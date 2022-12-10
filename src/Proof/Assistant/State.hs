{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.State where

import Control.Concurrent.STM (TBQueue, newTBQueueIO)

import Proof.Assistant.Request (InterpreterRequest)
import Proof.Assistant.Settings

-- | Simple state with settings and queue as communication model between bot and backend. 
data InterpreterState settings = InterpreterState
  { settings :: !settings
  , input    :: !(TBQueue InterpreterRequest)
  }

-- | Initialise a state based on its settings.
newInterpreterState
  :: forall settings. ToInterpreterState settings => settings -> IO (InterpreterState settings)
newInterpreterState settings = do
  input <- newTBQueueIO (getQueueSize settings)
  pure InterpreterState{..}
