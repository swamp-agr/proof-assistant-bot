{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Response where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Telegram.Bot.API
  (ChatId, MessageId, ParseMode (..), SendMessageRequest (..), SomeChatId (..))

import Proof.Assistant.Request

-- | Response for Telegram.
data InterpreterResponse = InterpreterResponse
  { interpreterResponseTelegramChatId :: !ChatId -- ^ Telegram ChatId (for reply).
  , interpreterResponseTelegramMessageId :: !MessageId -- ^ Telegram MessageId (for reply).
  , interpreterResponseResponse :: !ByteString -- ^ output data.
  }

-- | Cast 'InterpreterResponse' to 'SendMessageRequest'.
-- If first argument is 'True'
-- then it will wrap message in Monospace font and mark it with @MarkdownV2@ parse mode.
-- Otherwise, text message will be sent.
-- For all responses from Backends 'True' should be specified.
toSendMessageRequest :: Bool -> InterpreterResponse -> SendMessageRequest
toSendMessageRequest isMonospace InterpreterResponse{..} = SendMessageRequest
  { sendMessageChatId                   = SomeChatId interpreterResponseTelegramChatId
  , sendMessageText
      = if isMonospace
        then "```\n" <> decodeUtf8 interpreterResponseResponse <> "\n```\n"
        else decodeUtf8 interpreterResponseResponse <> "\n"
  , sendMessageParseMode                = if isMonospace then Just MarkdownV2 else Nothing
  , sendMessageEntities                 = Nothing
  , sendMessageDisableWebPagePreview    = Nothing
  , sendMessageDisableNotification      = Nothing
  , sendMessageProtectContent           = Nothing
  , sendMessageReplyToMessageId         = Just interpreterResponseTelegramMessageId
  , sendMessageAllowSendingWithoutReply = Nothing
  , sendMessageReplyMarkup              = Nothing
  }

-- | Cast 'InterpreterRequest' and output data to 'InterpreterResponse'.
makeTelegramResponse :: InterpreterRequest -> ByteString -> InterpreterResponse
makeTelegramResponse InterpreterRequest{..} response =
  InterpreterResponse
    { interpreterResponseTelegramChatId    = interpreterRequestTelegramChatId
    , interpreterResponseTelegramMessageId = interpreterRequestTelegramMessageId
    , interpreterResponseResponse          = response
    }
