{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Idris where

import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)

import Idris.Interaction.Command
import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

import qualified Data.ByteString.Char8 as BS8

-- | Call Idris 2 as CLI application.
-- It prepares the CLI command, executes it and waits for response.
callIdris2 :: InterpreterState IdrisSettings -> InterpreterRequest -> IO ByteString
callIdris2 InterpreterState{..} ir@InterpreterRequest{..}
  = case parseRequest interpreterRequestMessage of
      Left err -> pure err
      Right (ecmd, request) -> do
        let s@ExternalInterpreterSettings{..} = coerce settings
        action <- chooseCommand s ir ecmd request
        let asyncExecutable = do
              setPriority priority
              (_exitCode, stdout, stderr) <- action
              let response = unlines [if stdout == "\n" then "Done." else stdout, stderr]
              pure $ toBS response
            asyncTimer = asyncWait time
        eresult <- race asyncTimer (handleErrorMaybe asyncExecutable)
        case eresult of
          Left ()  -> pure "Time limit exceeded"
          Right bs -> pure bs

-- | Parse command. It could be unknown command or 'IdrisCommand' sub-command with its arguments
-- or even expression to evaluate.
parseRequest :: ByteString -> Either ByteString (Either () IdrisCommand, ByteString)
parseRequest rawCmd = case BS8.words rawSubCommand of
  [] -> Left "empty command"
  cmd : _ -> if BS8.isPrefixOf "/" cmd
    then case matchSupported cmd of
           Nothing -> Left $ "Unknown command: " <> cmd
           Just aCmd -> Right (Right aCmd, dropCommand rawSubCommand)
    else Right (Left (), rawSubCommand)
  where
    rawSubCommand = dropCommand rawCmd
