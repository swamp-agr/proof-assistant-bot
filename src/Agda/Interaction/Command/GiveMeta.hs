{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.GiveMeta where

import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Pretty (prettyTCM)
import Data.ByteString (ByteString)

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.BasicOps (give)
import Agda.Interaction.Command.Internal.Parser
import Proof.Assistant.Helpers (toBS)

giveMeta :: [ByteString] -> TCM ByteString
giveMeta s | length s >= 2 = do
  expr <- actOnMeta s $ \ ii e -> give WithoutForce ii Nothing e
  r <- prettyTCM expr
  pure $ toBS $ prettyShow r
giveMeta _ = pure $ "give" <> " metaid expr"
