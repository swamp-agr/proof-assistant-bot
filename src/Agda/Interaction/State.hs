{-# LANGUAGE RecordWildCards #-}

module Agda.Interaction.State where

import Agda.Interaction.Options
import Agda.Syntax.Translation.ConcreteToAbstract (importPrimitives)
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

-- | Agda has its own env ('TCEnv') and state ('TCState').
-- We are using them in combination with communication channel between Agda and bot.
data AgdaState = AgdaState
  { interpreterState :: !(InterpreterState AgdaSettings)
  , agdaEnvRef       :: !(IORef TCEnv)
  , agdaStateRef     :: !(IORef TCState)
  }

-- | Initiate 'AgdaState' from 'AgdaSettings'.
newAgdaState :: AgdaSettings -> IO AgdaState
newAgdaState settings = do
  interpreterState <- newInterpreterState $ settings
  let state0 = initState
      env = initEnv
  tmpDir <- getTemporaryDirectory
  (_, state1) <- withCurrentDirectory tmpDir
    $ runTCM env state0
    $ setCommandLineOptions defaultOptions >> importPrimitives
    
  agdaStateRef <- newIORef state1
  agdaEnvRef <- newIORef env
  pure AgdaState {..}

-- | Helper to get access to 'TCState'.
readTCState :: AgdaState -> IO TCState
readTCState = readIORef . agdaStateRef

-- | Helper to store new 'TCState'.
writeTCState :: TCState -> AgdaState -> IO ()
writeTCState tcState state = atomicWriteIORef (agdaStateRef state) $! tcState

-- | Helper to get access to 'TCEnv'.
readTCEnv :: AgdaState -> IO TCEnv
readTCEnv = readIORef . agdaEnvRef

-- | Helper to store new 'TCEnv'.
writeTCEnv :: TCEnv -> AgdaState -> IO ()
writeTCEnv tcEnv state = atomicWriteIORef (agdaEnvRef state) $! tcEnv

-- | Run chosen typechecking action in the typechecking monad ('TCM').
runAgda :: AgdaState -> TCM ByteString -> IO ByteString
runAgda as action = do
  tcState <- readTCState as
  tcEnv   <- readTCEnv as
  (response, newTCState) <- runTCM tcEnv tcState (action `catchError` catchAgdaError)
  writeTCState newTCState as
  pure response

-- | Catch error from Agda and make it looks pretty.
catchAgdaError :: MonadTCM m => TCErr -> m ByteString
catchAgdaError e = do
  s <- prettyError e
  return $ toBS s

-- | Helper to modify 'TCEnv'.
setEnv :: AgdaState -> (TCEnv -> TCEnv) -> IO ()
setEnv state modifier =
  let modify st = (modifier st, ())
  in atomicModifyIORef' (agdaEnvRef state) $ modify
