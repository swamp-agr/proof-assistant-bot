module Agda.Interaction.Command.TypeOf where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Interaction.BasicOps (typeInCurrent)
import Agda.TypeChecking.Monad.Base (TCM)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.Pretty (prettyShow)
import Data.ByteString (ByteString)

import Agda.Interaction.Command.Internal.Parser

import qualified Data.ByteString.Char8 as BS8

typeOf :: [ByteString] -> TCM ByteString
typeOf s =
    do  e  <- parseExpr (BS8.unpack $ BS8.unwords s)
        _e0 <- typeInCurrent Normalised e
        e1 <- typeInCurrent AsIs e
        r <- prettyTCM e1
        pure . toBS $ prettyShow r
