{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Bot where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import Proof.Assistant.Interpreter
import Proof.Assistant.Request
import Proof.Assistant.Response
import Proof.Assistant.Settings
import Agda.Interaction.State
import Proof.Assistant.Transport

type Model = BotState

data Action = Call Backend InterpreterRequest | SendBack InterpreterResponse | Debug String

data Backend = Agda | Arend | Idris | Coq | Lean | Rzk

proofAssistantBot :: Model -> BotApp Model Action
proofAssistantBot state = BotApp
  { botInitialModel = state
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction BotState{..} update
  | isCommand "coq" update = Call <$> Just Coq <*> updateToRequest update
  | isCommand "agda" update = Call <$> Just Agda <*> updateToRequest update
  | isCommand "idris2" update = Call <$> Just Idris <*> updateToRequest update
  | isCommand "lean" update = Call <$> Just Lean <*> updateToRequest update
  | isCommand "arend" update = Call <$> Just Arend <*> updateToRequest update
  | isCommand "rzk" update = Call <$> Just Rzk <*> updateToRequest update
  | otherwise = Just $ Debug $ show update
  where
    updateToRequest upd =
      InterpreterRequest
        <$> updateChatId upd  -- chatId
        <*> (fmap messageMessageId . extractUpdateMessage) upd -- messageId
        <*> (encodeUtf8 <$> updateMessageText upd)
    Settings{..} = botSettings
    isCommand cmd = isJust . parseUpdate (commandWithBotName botName cmd)

handleAction :: Action -> Model -> Eff Action Model
handleAction (Call backend request) model = model <# do
  liftIO $ do
    let BotState{..} = model
        Interpreters{..} = interpreters
        handle = writeInput request
        handleAgda = handle . interpreterState
    case backend of
      Agda -> handleAgda agda
      Arend -> handle arend
      Coq -> handle coq
      Idris -> handle idris
      Lean -> handle lean
      Rzk -> handle rzk
handleAction (SendBack response) model = model <# do
  let req = toSendMessageRequest response
      waitAndRetry result = if responseOk result
        then pure ()
        else case responseParameters result >>= responseParametersRetryAfter of
               Nothing -> pure ()
               Just sec -> do
                 liftIO . threadDelay $ coerce sec * 1000000
                 void (sendMessage req)
  liftClientM $ do
    result <- sendMessage req
    waitAndRetry result
handleAction (Debug str) model = model <# (liftIO $ putStrLn str)

runTelegramBot :: Model -> IO ()
runTelegramBot state@BotState{..} = do
  env <- defaultTelegramClientEnv (Token $ botToken botSettings)
  botActionFun <- startBotAsync (proofAssistantBot state) env
  void $ runConcurrently $ 
    Concurrently (botResponseHandlerJob botActionFun) *>
    Concurrently (interpreterJobs interpreters)
  where
    botResponseHandlerJob fun = forever $ do
      interpreterResponse <- readOutput state
      fun (SendBack interpreterResponse)
    interpreterJobs Interpreters{..} = void $ runConcurrently $
      Concurrently (runInterpreter state agda) *>
      Concurrently (runInterpreter state arend) *>
      Concurrently (runInterpreter state coq) *>
      Concurrently (runInterpreter state idris) *>
      Concurrently (runInterpreter state lean) *>
      Concurrently (runInterpreter state rzk)

run :: IO ()
run = runTelegramBot =<< newBotState =<< loadDefaultSettings
