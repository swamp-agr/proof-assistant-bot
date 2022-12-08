{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.TypeIn where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Interaction.BasicOps (typeInMeta)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.Pretty (prettyShow)
import Data.ByteString (ByteString)

import Agda.Interaction.Command.Internal.Parser
import Proof.Assistant.Helpers (toBS)

typeIn :: [ByteString] -> TCM ByteString
typeIn s@(_:_:_) =
    actOnMeta s $ \i e ->
    do  e1 <- typeInMeta i Normalised e
        _e2 <- typeInMeta i AsIs e
        r <- prettyTCM e1
        pure . toBS $ prettyShow r

typeIn _ = pure ":typeIn meta expr"
