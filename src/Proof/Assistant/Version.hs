{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Version where

import Data.Text (Text)
import Data.Version
import Paths_proof_assistant_bot

import qualified Data.Text as Text

rzkVersion :: String
rzkVersion = VERSION_rzk

proofAssistantBotVersion :: String
proofAssistantBotVersion = showVersion version

makeVersion :: Text -> Text
makeVersion txt = Text.unlines
  [ Text.replace "<bot_version>" (Text.pack proofAssistantBotVersion) txt
  , "rzk v." <> Text.pack rzkVersion
  ]
