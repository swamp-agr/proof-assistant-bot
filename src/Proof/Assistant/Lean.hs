{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Lean where

import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Text (unpack)
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

import qualified Data.Text as Text

callLean :: InterpreterState LeanSettings -> InterpreterRequest -> IO ByteString
callLean InterpreterState{..} ir = do
  let ls@LeanSettings{..} = coerce settings
      s@ExternalInterpreterSettings{..} = externalLean
  (dir, path) <- refreshTmpFile s (validateLean ls ir) (Just projectDir)
  withCurrentDirectory dir $ do
    let runProcess = runWithSandboxMaybe sandbox executable args
        asyncExecutable = do
          setPriority priority
          (_exitCode, stdout, stderr) <- runProcess
          pure . validate path . toBS . unlines $ [stdout, stderr]
        asyncTimer = asyncWait time
    eresult <- race asyncTimer (handleErrorMaybe asyncExecutable)
    case eresult of
      Left ()  -> pure "Time limit exceeded"
      Right bs -> pure bs

validateLean :: LeanSettings -> InterpreterRequest -> InterpreterRequest
validateLean LeanSettings{..} ir@InterpreterRequest{..}
  = ir { interpreterRequestMessage = validatedMsg }
  where
    remove txt blockedPrefix = Text.replace blockedPrefix "" txt
    removeAllFromLine x = foldl' remove x leanBlockList
    removeUnsafeImports = textToBS . removeAllFromLine . bsToText

    validatedMsg = removeUnsafeImports interpreterRequestMessage

runWithSandboxMaybe
  :: Maybe SandboxSettings -> Executable -> CmdArgs -> IO (ExitCode, String, String)
runWithSandboxMaybe Nothing exec arguments
  = readProcessWithExitCode (t2s exec) (unpack <$> coerce arguments) ""
runWithSandboxMaybe (Just SandboxSettings{..}) exec arguments
  = readProcessWithExitCode (t2s sandboxExecutable) fullArgsList ""
  where
    sandboxArgsList = unpack <$> coerce sandboxArgs
    appArgsList = t2s exec : (unpack <$> coerce arguments)
    fullArgsList = concat [sandboxArgsList, ["--"], appArgsList]
