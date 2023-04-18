{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.Helpers where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Coerce (Coercible, coerce)
import Data.Char (isSpace)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Dhall (Natural)

import Proof.Assistant.Settings (Time (..))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Cast something to 'Int'.
toInt :: forall a b. (Num b, Integral b, Coercible a b) => a -> Int
toInt = fromIntegral . coerce @_ @b

-- | Cast something to 'String'.
t2s :: Coercible a Text => a -> String
t2s = Text.unpack . coerce

-- | Cast something as 'Text'.
s2t :: Show a => a -> Text
s2t = Text.pack . show

-- | Wait N seconds.
asyncWait :: Time -> IO ()
asyncWait n = threadDelay (toInt @_ @Natural n * 1000000)

-- | If input starts with @/@ drop the whole command with a space and return the remaining part.
dropCommand :: ByteString -> ByteString
dropCommand xs = if BS8.take 1 xs == "/" then drop' xs else xs
  where
    drop' = BS8.dropWhile isSpace . BS8.dropWhile (not . isSpace)

-- | Drop not only command but sub-command too.
dropSubCommand :: ByteString -> ByteString
dropSubCommand = dropCommand . dropCommand

-- | Cast UTF-8 'String' to 'ByteString'.
toBS :: String -> ByteString
toBS = Text.encodeUtf8 . Text.pack

-- | Cast UTF-8 'ByteString' to 'String'.
fromBS :: ByteString -> String
fromBS = Text.unpack . Text.decodeUtf8

-- | Cast UTF-8 'ByteString' to 'Text'.
bsToText :: ByteString -> Text
bsToText = Text.decodeUtf8

-- | Cast UTF-8 'ByteString' to 'Text'.
textToBS :: Text -> ByteString
textToBS = Text.encodeUtf8

-- | Cast some exception to 'ByteString'.
processError :: SomeException -> IO ByteString
processError (SomeException ex) = pure (toBS $ show ex)

-- | Wrap IO action with 'processError' and return error message as 'ByteString'.
handleErrorMaybe :: IO ByteString -> IO ByteString
handleErrorMaybe = (`catch` processError)
