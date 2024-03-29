{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Version where

import Data.Text (Text)
import Data.Version
import Paths_proof_assistant_bot

import qualified Data.Text as Text

-- | Version of Agda.
agdaVersion :: String
agdaVersion = VERSION_Agda

-- | Version of Rzk.
rzkVersion :: String
rzkVersion = VERSION_rzk

-- | Version of this package.
proofAssistantBotVersion :: String
proofAssistantBotVersion = showVersion version

-- | Helper to build version from template message.
makeVersion :: Text -> Text
makeVersion txt = Text.unlines
  [ Text.replace "<bot_version>" (Text.pack proofAssistantBotVersion)
    $ Text.replace "<agda_version>" (Text.pack agdaVersion) txt
  , "rzk v." <> Text.pack rzkVersion
  ]
