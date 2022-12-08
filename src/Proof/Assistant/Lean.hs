{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Lean where

import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (unpack)
import System.Directory (withCurrentDirectory)
import System.Process (readProcessWithExitCode)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

callLean :: InterpreterState LeanSettings -> InterpreterRequest -> IO ByteString
callLean InterpreterState{..} ir = do
  let LeanSettings{..} = coerce settings
      s@ExternalInterpreterSettings{..} = external
  (dir, path) <- refreshTmpFile s ir (Just projectDir)
  withCurrentDirectory dir $ do
    let fullArgs = (unpack <$> coerce args) <> [path]
        runProcess = readProcessWithExitCode (t2s executable) fullArgs ""
        asyncExecutable = do
          setPriority priority
          (exitCode, stdout, stderr) <- runProcess
          let response = unlines [stdout, stderr]
          putStrLn $ show exitCode <> " " <> response
          pure $ toBS response
        asyncTimer = asyncWait time
    eresult <- race asyncTimer asyncExecutable
    case eresult of
      Left ()  -> pure "Time limit exceeded"
      Right bs -> pure bs
