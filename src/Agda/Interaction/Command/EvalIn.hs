{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.EvalIn where

import Agda.Interaction.Base (ComputeMode (..))
import Agda.Interaction.BasicOps (evalInCurrent)
import Agda.Syntax.Abstract.Pretty (prettyA)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.Utils.Pretty (prettyShow)
import Data.ByteString (ByteString)

import Agda.Interaction.Command.Internal.Parser

evalIn :: [ByteString] -> TCM ByteString
evalIn s | length s >= 2 =
    do  d <- actOnMeta s $ \_ e -> prettyA =<< evalInCurrent DefaultCompute e
        pure . toBS . prettyShow $ d
evalIn _ = pure ":eval metaid expr"
