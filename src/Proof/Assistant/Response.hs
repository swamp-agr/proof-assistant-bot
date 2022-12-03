{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Response where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Telegram.Bot.API
  (ChatId, MessageId, ParseMode (..), SendMessageRequest (..), SomeChatId (..))

import Proof.Assistant.Request

data InterpreterResponse = InterpreterResponse
  { interpreterResponseTelegramChatId :: !ChatId
  , interpreterResponseTelegramMessageId :: !MessageId
  , interpreterResponseResponse :: !ByteString
  }

toSendMessageRequest :: InterpreterResponse -> SendMessageRequest
toSendMessageRequest InterpreterResponse{..} = SendMessageRequest
  { sendMessageChatId                = SomeChatId interpreterResponseTelegramChatId
  , sendMessageText
      = "```\n" <> decodeUtf8 interpreterResponseResponse <> "\n```\n"
  , sendMessageParseMode                = Just MarkdownV2
  , sendMessageEntities                 = Nothing
  , sendMessageDisableWebPagePreview    = Nothing
  , sendMessageDisableNotification      = Nothing
  , sendMessageProtectContent           = Nothing
  , sendMessageReplyToMessageId         = Just interpreterResponseTelegramMessageId
  , sendMessageAllowSendingWithoutReply = Nothing
  , sendMessageReplyMarkup              = Nothing
  }

makeTelegramResponse :: InterpreterRequest -> ByteString -> InterpreterResponse
makeTelegramResponse InterpreterRequest{..} response =
  InterpreterResponse
    { interpreterResponseTelegramChatId    = interpreterRequestTelegramChatId
    , interpreterResponseTelegramMessageId = interpreterRequestTelegramMessageId
    , interpreterResponseResponse          = response
    }
