module Agda.Interaction.Command.RefineMeta where

import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Pretty (prettyTCM)
import Data.ByteString (ByteString)

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.BasicOps (refine)
import Agda.Interaction.Command.Internal.Parser
import Proof.Assistant.Helpers (toBS)

refineMeta :: [ByteString] -> TCM ByteString
refineMeta s | length s >= 2 = do
  expr <- actOnMeta s $ \ ii e -> refine WithoutForce ii Nothing e
  r <- prettyTCM expr
  pure $ toBS $ prettyShow r
refineMeta _ = pure $ toBS $ ": refine" <> " metaid expr"
