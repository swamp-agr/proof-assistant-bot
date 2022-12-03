module Proof.Assistant.Request where

import Data.ByteString (ByteString)
import Telegram.Bot.API (ChatId, MessageId)

data InterpreterRequest = InterpreterRequest
  { interpreterRequestTelegramChatId :: !ChatId
  , interpreterRequestTelegramMessageId :: !MessageId
  , interpreterRequestMessage :: !ByteString
  }
