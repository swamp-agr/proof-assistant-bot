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

data Interpreters = Interpreters
  { agda  :: !AgdaState
  , arend :: !ExternalState
  , idris :: !(InterpreterState IdrisSettings)
  , coq   :: !ExternalState
  , lean  :: !(InterpreterState LeanSettings)
  , rzk   :: !InternalState
  }

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

data ChatInfo = ChatInfo
  { filePrefix      :: !FilePath
  , sourceDirectory :: !FilePath
  }

data BotState = BotState
  { output       :: !(TBQueue InterpreterResponse)
  , botSettings  :: !Settings
  , interpreters :: !Interpreters
  }

newBotState :: Settings -> IO BotState
newBotState botSettings@Settings{..} = do
  output <- newTBQueueIO outputSize
  interpreters <- newInterpreters interpretersSettings
  pure BotState{..}

readInput :: InterpreterState settings -> IO InterpreterRequest
readInput state = atomically $! readTBQueue (input state)

readOutput :: BotState -> IO InterpreterResponse
readOutput state = atomically $! readTBQueue (output state)

writeInput :: InterpreterRequest -> InterpreterState settings -> IO ()
writeInput message state = atomically $! writeTBQueue (input state) message

writeOutput :: InterpreterResponse -> BotState -> IO ()
writeOutput message state = atomically $! writeTBQueue (output state) message
