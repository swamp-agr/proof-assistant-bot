{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.RefreshFile where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import System.Directory
import System.FilePath
import Telegram.Bot.API (ChatId (..))

import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Settings

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text

-- | Most interpreters work with files. Therefore, we need to store user input as a file.
-- Remember that input could come from different chats, so we need to store input separately.
-- Unless directory specified, temporary directory will be used to store the files.
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

-- | Make absolute filepath based on settings, request and directory.
getTempFilePath :: ExternalInterpreterSettings -> InterpreterRequest -> FilePath -> FilePath
getTempFilePath
  ExternalInterpreterSettings{tempFilePrefix, fileExtension}
  InterpreterRequest{interpreterRequestTelegramChatId} dir =
    let tmpFilepath = dir
          </> tempFilePrefix
          <> chatIdToString interpreterRequestTelegramChatId
          <.> fileExtension
    in tmpFilepath

-- | Helper to convert Telegram ChatId to 'String' (for filepath).
chatIdToString :: ChatId -> String
chatIdToString = show . coerce @_ @Integer

-- | Helper to cut filepath from the output.
validate :: FilePath -> ByteString -> ByteString
validate path bs = textToBS (Text.replace textPath "<bot>" txt)
  where
    textPath = Text.pack path
    txt = bsToText bs
