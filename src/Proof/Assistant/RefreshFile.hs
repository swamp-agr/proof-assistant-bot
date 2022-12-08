{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.RefreshFile where

import Data.Coerce (coerce)
import System.Directory
import System.FilePath
import Telegram.Bot.API (ChatId (..))

import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Settings

import qualified Data.ByteString.Char8 as BS8

refreshTmpFile
  :: ExternalInterpreterSettings
  -> InterpreterRequest
  -> Maybe FilePath
  -> IO (FilePath, FilePath)
refreshTmpFile
  settings
  ir@InterpreterRequest{interpreterRequestMessage} mDir = do
    tmpDir <- maybe getTemporaryDirectory pure mDir
    let tmpFilepath = getTempFilePath settings ir tmpDir
        createFile = do
          BS8.writeFile tmpFilepath $ dropSubCommand interpreterRequestMessage
          pure (tmpDir, tmpFilepath)
    exist <- doesFileExist tmpFilepath
    if (not exist)
      then createFile
      else removeFile tmpFilepath >> createFile

getTempFilePath :: ExternalInterpreterSettings -> InterpreterRequest -> FilePath -> FilePath
getTempFilePath
  ExternalInterpreterSettings{tempFilePrefix, fileExtension}
  InterpreterRequest{interpreterRequestTelegramChatId} dir =
    let chatIdToString = show . coerce @_ @Integer
        tmpFilepath = dir
          </> tempFilePrefix
          <> chatIdToString interpreterRequestTelegramChatId
          <.> fileExtension
    in tmpFilepath
