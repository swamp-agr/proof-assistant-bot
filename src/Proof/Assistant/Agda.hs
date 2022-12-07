{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.Agda where

import Control.Concurrent.Async
import Control.Exception
import Data.ByteString (ByteString)
import Data.Coerce
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>), (<.>))
import System.Mem
import Telegram.Bot.API (ChatId (..))

import Agda.Interaction.Command
import Agda.Interaction.State
import Agda.TypeChecking.Monad.Base (envCurrentPath)
import Agda.Utils.FileName (absolute)
import Proof.Assistant.Helpers
import Proof.Assistant.Request
import Proof.Assistant.Settings
import Proof.Assistant.State

import qualified Data.ByteString.Char8 as BS8

callAgda :: AgdaState -> InterpreterRequest -> IO ByteString
callAgda currentAgdaState@AgdaState{..} InterpreterRequest{..} = do
  let InternalInterpreterSettings{..} = internal (settings interpreterState)
      asyncApi = withChat interpreterRequestTelegramChatId currentAgdaState $ do
        enableAllocationLimit
        setAllocationCounter (fromIntegral allocations)
        interpretAgda currentAgdaState interpreterRequestMessage

      catchAsyncApi :: SomeException -> IO ByteString
      catchAsyncApi (SomeException ex) = pure (BS8.pack $ show ex)
      asyncTimer = asyncWait (coerce timeout)
  eresult <- race asyncTimer (asyncApi `catch` catchAsyncApi)
  case eresult of
    Left () -> pure "Time limit exceeded"
    Right result -> pure result

withChat :: ChatId -> AgdaState -> IO a -> IO a
withChat chatId state action = do
  tmpDir <- getTemporaryDirectory
  let InternalInterpreterSettings{..} = (internal . settings . interpreterState) state
      chatIdToString = show . coerce @ChatId @Integer
      sourceFile = tmpDir
        </> sourceFilePrefix
        <> chatIdToString chatId
        <.> sourceFileExtension
  absSourceFile <- absolute sourceFile
  let modifiedEnv s = s { envCurrentPath = Just absSourceFile }
  setEnv state modifiedEnv
  action

interpretAgda :: AgdaState -> ByteString -> IO ByteString
interpretAgda state req = case parseRequest req of
    Left err -> pure err
    Right (ecmd, request) -> do
      let fetchResponse = chooseCommand state ecmd
      runAgda state =<< fetchResponse request

parseRequest :: ByteString -> Either ByteString (Either () AgdaCommand, ByteString)
parseRequest rawCmd = case BS8.words rawSubCommand of
  [] -> Left "Empty command"
  cmd : _   -> if BS8.isPrefixOf "/" cmd
    then case matchSupported cmd of
           Nothing -> Left $ "Unknown command: " <> cmd
           Just aCmd -> Right (Right aCmd, dropCommand rawSubCommand)
    else Right (Left (), rawSubCommand)
  where
    rawSubCommand = dropCommand rawCmd
