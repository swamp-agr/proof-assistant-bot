{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.Helpers where

import Control.Concurrent (threadDelay)
import Data.Coerce (Coercible, coerce)
import Data.Char (isSpace)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Dhall (Natural)

import Proof.Assistant.Settings (Time (..))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

toInt :: forall a b. (Num b, Integral b, Coercible a b) => a -> Int
toInt = fromIntegral . coerce @_ @b


t2s :: Coercible a Text => a -> String
t2s = Text.unpack . coerce

asyncWait :: Time -> IO ()
asyncWait n = threadDelay (toInt @_ @Natural n * 1000000)

dropCommand :: ByteString -> ByteString
dropCommand xs = if BS8.take 1 xs == "/" then drop' xs else xs
  where
    drop' = BS8.dropWhile isSpace . BS8.dropWhile (not . isSpace)

dropSubCommand :: ByteString -> ByteString
dropSubCommand = dropCommand . dropCommand

toBS :: String -> ByteString
toBS = Text.encodeUtf8 . Text.pack

fromBS :: ByteString -> String
fromBS = Text.unpack . Text.decodeUtf8

bsToText :: ByteString -> Text
bsToText = Text.decodeUtf8

textToBS :: Text -> ByteString
textToBS = toBS . t2s
