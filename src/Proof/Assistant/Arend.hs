{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Arend where

import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (unpack)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath
import System.Process (readProcessWithExitCode)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.IO as Text

-- | Call Arend typechecker as CLI application.
-- It prepares the CLI command, executes it and waits for response.
callArend :: InterpreterState ArendSettings -> InterpreterRequest -> IO ByteString
callArend InterpreterState{..} ir = do
  let ArendSettings{..} = coerce settings
      ExternalInterpreterSettings{..} = externalArend
  currentProjectDir <- setArendProject settings ir
  withCurrentDirectory arendRootProjectDir $ do
    let fullArgs = (unpack <$> coerce args) <> [currentProjectDir]
        runProcess = readProcessWithExitCode (t2s executable) fullArgs ""
        asyncExecutable = do
          setPriority priority
          (_exitCode, stdout, stderr) <- runProcess
          pure . validate currentProjectDir . toBS . unlines $ [stdout, stderr]
        asyncTimer = asyncWait time
    eresult <- race asyncTimer (handleErrorMaybe asyncExecutable)
    case eresult of
      Left ()  -> pure "Time limit exceeded"
      Right bs -> pure bs

-- | Arend requires a project for every file to typecheck.
-- It reads project configuration, finds all files in a project directory
-- and tries to typecheck all of them.
-- 
-- Since we need to isolate different chats from each other we need to ensure
-- that every chat has its own project. And every project has its own configuration.
--
-- As result, project directory will be created, set and returned.
setArendProject :: ArendSettings -> InterpreterRequest -> IO FilePath
setArendProject ArendSettings{..} InterpreterRequest{..} = do
  let ExternalInterpreterSettings{..} = externalArend
      projectDir = arendRootProjectDir </> (tempFilePrefix  <> chatIdToString interpreterRequestTelegramChatId)
      projectConfigPath = projectDir </> arendYamlFilename
      projectSourcePath = projectDir </> arendModuleName <.> fileExtension
  createDirectoryIfMissing True projectDir
  Text.writeFile projectConfigPath arendYamlContent
  BS8.writeFile projectSourcePath $ dropSubCommand interpreterRequestMessage
  pure projectDir
  
