{-# LANGUAGE RecordWildCards #-}

module Agda.Interaction.State where

import Agda.Interaction.Options
import Agda.TypeChecking.Errors (prettyError)
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Options (setCommandLineOptions)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef, atomicModifyIORef')
import System.Directory (getTemporaryDirectory, withCurrentDirectory)

import Proof.Assistant.Helpers (toBS)
import Proof.Assistant.Settings
import Proof.Assistant.State

data AgdaState = AgdaState
  { interpreterState :: !(InterpreterState AgdaSettings)
  , agdaEnvRef       :: !(IORef TCEnv)
  , agdaStateRef     :: !(IORef TCState)
  }

newAgdaState :: AgdaSettings -> IO AgdaState
newAgdaState settings = do
  interpreterState <- newInterpreterState $ settings
  let state0 = initState
      env = initEnv
  tmpDir <- getTemporaryDirectory
  (_, state1) <- withCurrentDirectory tmpDir
    $ runTCM env state0
    $ setCommandLineOptions defaultOptions
  agdaStateRef <- newIORef state1
  agdaEnvRef <- newIORef env
  pure AgdaState {..}

readTCState :: AgdaState -> IO TCState
readTCState = readIORef . agdaStateRef

writeTCState :: TCState -> AgdaState -> IO ()
writeTCState tcState state = atomicWriteIORef (agdaStateRef state) $! tcState

readTCEnv :: AgdaState -> IO TCEnv
readTCEnv = readIORef . agdaEnvRef

writeTCEnv :: TCEnv -> AgdaState -> IO ()
writeTCEnv tcEnv state = atomicWriteIORef (agdaEnvRef state) $! tcEnv

runAgda :: AgdaState -> TCM ByteString -> IO ByteString
runAgda as action = do
  tcState <- readTCState as
  tcEnv   <- readTCEnv as
  (response, newTCState) <- runTCM tcEnv tcState (action `catchError` catchAgdaError)
  writeTCState newTCState as
  pure response

catchAgdaError :: MonadTCM m => TCErr -> m ByteString
catchAgdaError e = do
  s <- prettyError e
  return $ toBS s

setEnv :: AgdaState -> (TCEnv -> TCEnv) -> IO ()
setEnv state modifier =
  let modify st = (modifier st, ())
  in atomicModifyIORef' (agdaEnvRef state) $ modify
