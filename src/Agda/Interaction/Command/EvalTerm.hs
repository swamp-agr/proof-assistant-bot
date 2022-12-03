module Agda.Interaction.Command.EvalTerm where

import Agda.Interaction.Command.Internal.Parser 

import Agda.Interaction.Base (ComputeMode (..))
import Agda.Interaction.BasicOps hiding (parseExpr)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.Pretty (prettyShow)

import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS8

evalTerm :: ByteString -> TCM ByteString
evalTerm s = do
  e <- parseExpr (BS8.unpack s)
  v <- evalInCurrent DefaultCompute e
  r <- prettyTCM v
  pure . toBS $ prettyShow r
