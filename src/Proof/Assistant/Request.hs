module Proof.Assistant.Request where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Telegram.Bot.API
import Telegram.Bot.Simple.UpdateParser

-- | Request from Telegram.
data InterpreterRequest = InterpreterRequest
  { interpreterRequestTelegramChatId :: !ChatId -- ^ Telegram ChatId (for reply and isolation).
  , interpreterRequestTelegramMessageId :: !MessageId -- ^ Telegram MessageId (for reply).
  , interpreterRequestMessage :: !ByteString -- ^ Message content.
  }

-- | Cast Telegram Update to 'InterpreterRequest'.
updateToRequest :: Update -> Maybe InterpreterRequest
updateToRequest upd = InterpreterRequest
  <$> updateChatId upd -- chatId
  <*> (fmap messageMessageId . extractUpdateMessage) upd -- messageId
  <*> (encodeUtf8 <$> updateMessageText upd)
