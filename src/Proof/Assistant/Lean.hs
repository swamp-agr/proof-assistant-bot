{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Lean where

import Control.Concurrent.Async (race)
import Data.Coerce (coerce)
import Data.Text (unpack)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.Response
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

callLean :: InterpreterState LeanSettings -> InterpreterRequest -> IO BotResponse
callLean InterpreterState{..} ir = do
  let LeanSettings{..} = coerce settings
      s@ExternalInterpreterSettings{..} = externalLean
  (dir, path) <- refreshTmpFile s ir (Just projectDir)
  withCurrentDirectory dir $ do
    let runProcess = runWithSandboxMaybe sandbox executable args path
        asyncExecutable = do
          setPriority priority
          (_exitCode, stdout, stderr) <- runProcess
          pure . validate path . toBS . unlines $ [stdout, stderr]
        asyncTimer = asyncWait time
    eresult <- race asyncTimer (handleErrorMaybe asyncExecutable)
    case eresult of
      Left ()  -> pure (TextResponse "Time limit exceeded")
      Right bs -> pure (TextResponse bs)

runWithSandboxMaybe
  :: Maybe SandboxSettings -> Executable -> CmdArgs -> FilePath -> IO (ExitCode, String, String)
runWithSandboxMaybe Nothing exec arguments path
  = readProcessWithExitCode (t2s exec) ((unpack <$> coerce arguments) <> [path]) ""
runWithSandboxMaybe (Just SandboxSettings{..}) exec arguments path
  = readProcessWithExitCode (t2s sandboxExecutable) fullArgsList ""
  where
    sandboxArgsList = unpack <$> coerce sandboxArgs
    appArgsList = t2s exec : (unpack <$> coerce arguments)
    fullArgsList = concat [sandboxArgsList, ["--"], appArgsList, [path]]
