{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Response where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Telegram.Bot.API
  (ChatId, ContentType, MessageId
  , InputFile (..), ParseMode (..), PhotoFile (..)
  , SendAnimationRequest (..), SendMessageRequest (..), SendPhotoRequest (..), SomeChatId (..)
  , defSendAnimation, defSendMessage, defSendPhoto
  )

import Proof.Assistant.Request

-- | Bot response: either text message or picture (filepath and its content type).
data BotResponse
  = TextResponse !ByteString
  | ImageResponse
      { imgResponseCType :: !ContentType
      , imgResponsePath :: !FilePath
      , imgResponeWidth :: !(Maybe Int)
      , imgResponseHeight :: !(Maybe Int)
      }

-- | Response for Telegram.
data InterpreterResponse = InterpreterResponse
  { interpreterResponseTelegramChatId :: !ChatId -- ^ Telegram ChatId (for reply).
  , interpreterResponseTelegramMessageId :: !MessageId -- ^ Telegram MessageId (for reply).
  , interpreterResponseResponse :: !BotResponse -- ^ output data.
  }

data TelegramMessage
  = TgMsg SendMessageRequest
  | TgPng SendPhotoRequest
  | TgGif SendAnimationRequest

-- | Cast 'InterpreterResponse' to either 'SendMessageRequest' or 'SendPhotoRequest'.
-- For text messages:
-- - If first argument is 'True'
-- then it will wrap message in Monospace font and mark it with @MarkdownV2@ parse mode.
-- - Otherwise, text message will be sent.
-- - For all responses from Backends 'True' should be specified.
-- 
-- For images content type should be specified.
toMessageRequest
  :: Bool -> InterpreterResponse -> TelegramMessage
toMessageRequest isMonospace InterpreterResponse{..} = case interpreterResponseResponse of
  TextResponse bytes -> TgMsg $ SendMessageRequest
    { sendMessageChatId                   = SomeChatId interpreterResponseTelegramChatId
    , sendMessageMessageThreadId          = Nothing
    , sendMessageText
        = if isMonospace
          then "```\n" <> decodeUtf8 bytes <> "\n```\n"
          else decodeUtf8 bytes <> "\n"
    , sendMessageParseMode                = if isMonospace then Just MarkdownV2 else Nothing
    , sendMessageEntities                 = Nothing
    , sendMessageDisableWebPagePreview    = Nothing
    , sendMessageDisableNotification      = Nothing
    , sendMessageProtectContent           = Nothing
    , sendMessageReplyToMessageId         = Just interpreterResponseTelegramMessageId
    , sendMessageAllowSendingWithoutReply = Nothing
    , sendMessageReplyMarkup              = Nothing
    }
  ImageResponse ctype imgPath mwidth mheight -> case ctype of
    "image/png" ->
      let reply = defSendPhoto (SomeChatId interpreterResponseTelegramChatId)
            $ MakePhotoFile $ InputFile imgPath ctype
      in TgPng (reply { sendPhotoReplyToMessageId = Just interpreterResponseTelegramMessageId })
    "image/gif" ->
      let reply = defSendAnimation (SomeChatId interpreterResponseTelegramChatId)
            $ InputFile imgPath "image/gif"
      in TgGif
        (reply { sendAnimationReplyToMessageId = Just interpreterResponseTelegramMessageId
               , sendAnimationWidth = mwidth
               , sendAnimationHeight = mheight
               })

    _ ->
      let reply =
            defSendMessage (SomeChatId interpreterResponseTelegramChatId) "Unsupported request"
      in TgMsg (reply { sendMessageReplyToMessageId = Just interpreterResponseTelegramMessageId })

-- | Cast 'InterpreterRequest' and output data to 'InterpreterResponse'.
makeTelegramResponse :: InterpreterRequest -> BotResponse -> InterpreterResponse
makeTelegramResponse InterpreterRequest{..} response =
  InterpreterResponse
    { interpreterResponseTelegramChatId    = interpreterRequestTelegramChatId
    , interpreterResponseTelegramMessageId = interpreterRequestTelegramMessageId
    , interpreterResponseResponse          = response
    }
