module Proof.Assistant.Request where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Telegram.Bot.API
import Telegram.Bot.Simple.UpdateParser

data InterpreterRequest = InterpreterRequest
  { interpreterRequestTelegramChatId :: !ChatId
  , interpreterRequestTelegramMessageId :: !MessageId
  , interpreterRequestMessage :: !ByteString
  }

updateToRequest :: Update -> Maybe InterpreterRequest
updateToRequest upd = InterpreterRequest
  <$> updateChatId upd -- chatId
  <*> (fmap messageMessageId . extractUpdateMessage) upd -- messageId
  <*> (encodeUtf8 <$> updateMessageText upd)
