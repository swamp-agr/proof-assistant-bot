{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Idris.Interaction.Command where

import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Text (unpack)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import System.Directory
import System.FilePath (takeFileName)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.Settings

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap

data IdrisCommand
  = Load | TypeOf | Help
  deriving (Eq, Show, Generic)

supportedCommands :: HashMap ByteString IdrisCommand
supportedCommands = HashMap.fromList
  [ (,) "/load" Load
  , (,) "/?" Help
  , (,) "/typeOf" TypeOf
  ]

matchSupported :: ByteString -> Maybe IdrisCommand
matchSupported = (`HashMap.lookup` supportedCommands)

chooseCommand
  :: ExternalInterpreterSettings
  -> InterpreterRequest
  -> Either () IdrisCommand -> ByteString -> IO (IO (ExitCode, String, String))
chooseCommand settings request ecmd input = case ecmd of
  Left () ->
    withResource settings request
      $ runProcess settings (BS8.unpack $ validateCmd input) . takeFileName
  Right cmd -> case cmd of
    Load -> do
      (dir, path) <- refreshTmpFile settings request Nothing
      pure $ withCurrentDirectory dir $ runProcess settings "main" $ takeFileName path
    TypeOf ->
      withResource settings request
        $ runProcess settings (":ti " <> BS8.unpack input) . takeFileName
    Help -> pure $ textResponse "TBD"

textResponse :: String -> IO (ExitCode, String, String)
textResponse txt = pure (ExitSuccess, txt, "")

withResource
  :: ExternalInterpreterSettings
  -> InterpreterRequest
  -> (FilePath -> IO (ExitCode, String, String))
  -> IO (IO (ExitCode, String, String))
withResource settings request action = do
  tmpDir <- getTemporaryDirectory
  let path = getTempFilePath settings request tmpDir
  pure $ withCurrentDirectory tmpDir $ do
    exist <- doesFileExist path
    if exist
      then action path
      else textResponse "Not found"

runProcess :: ExternalInterpreterSettings -> String -> FilePath -> IO (ExitCode, String, String)
runProcess ExternalInterpreterSettings{..} input path =
  readProcessWithExitCode (t2s executable) fullArgs ""
  where
    fullArgs = (unpack <$> coerce args) <> [input, path]

validateCmd :: ByteString -> ByteString
validateCmd xs =
  let cut = if BS8.take 1 xs == ":"
        then BS8.dropWhile isSpace . BS8.dropWhile (not . isSpace)
        else id
  in cut xs
