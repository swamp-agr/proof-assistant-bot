module Agda.Interaction.Command.ShowScope where

import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Monad.State (getScope)
import Agda.Utils.Pretty (prettyShow)
import Data.ByteString (ByteString)

import Proof.Assistant.Helpers (toBS)

showScope :: TCM ByteString
showScope = do
  scope <- getScope
  pure . toBS $ prettyShow scope
