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
    eresult <- race asyncTimer asyncExecutable
    case eresult of
      Left ()  -> pure "Time limit exceeded"
      Right bs -> pure bs
        
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
  
