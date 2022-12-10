{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Transport where

import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)

import Agda.Interaction.State

import Proof.Assistant.Request (InterpreterRequest)
import Proof.Assistant.Response (InterpreterResponse)
import Proof.Assistant.Settings hiding (InterpretersSettings)
import Proof.Assistant.State

import qualified Proof.Assistant.Settings as Settings

-- | Combination of all supported backend interpreters.
data Interpreters = Interpreters
  { agda  :: !AgdaState
  , arend :: !(InterpreterState ArendSettings)
  , idris :: !(InterpreterState IdrisSettings)
  , coq   :: !ExternalState
  , lean  :: !(InterpreterState LeanSettings)
  , rzk   :: !InternalState
  }

-- | Initiate new interpreters from settings.
newInterpreters :: Settings.InterpretersSettings -> IO Interpreters
newInterpreters settings = do
  agda <- newAgdaState $ Settings.agda settings
  arend <- newInterpreterState $ Settings.arend settings
  idris <- newInterpreterState $ Settings.idris settings
  coq <- newInterpreterState $ Settings.coq settings
  lean <- newInterpreterState $ Settings.lean settings
  rzk <- newInterpreterState $ Settings.rzk settings
  pure Interpreters{..}

type ExternalState = InterpreterState ExternalInterpreterSettings

type InternalState = InterpreterState InternalInterpreterSettings

-- | Bot has its own state.
data BotState = BotState
  { output       :: !(TBQueue InterpreterResponse) -- ^ Queue to read messages from backend interpreters.
  , botSettings  :: !Settings -- ^ Bot settings.
  , interpreters :: !Interpreters -- ^ All interpreters with their states.
  }

-- | Initiate 'BotState' from 'Settings'.
newBotState :: Settings -> IO BotState
newBotState botSettings@Settings{..} = do
  output <- newTBQueueIO outputSize
  interpreters <- newInterpreters interpretersSettings
  pure BotState{..}

-- | Read message from "input" queue.
readInput :: InterpreterState settings -> IO InterpreterRequest
readInput state = atomically $! readTBQueue (input state)

-- | Read message from "output" queue.
readOutput :: BotState -> IO InterpreterResponse
readOutput state = atomically $! readTBQueue (output state)

-- | Write message to "input" queue.
writeInput :: InterpreterRequest -> InterpreterState settings -> IO ()
writeInput message state = atomically $! writeTBQueue (input state) message

-- | Write message to "output" queue.
writeOutput :: InterpreterResponse -> BotState -> IO ()
writeOutput message state = atomically $! writeTBQueue (output state) message
