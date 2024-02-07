{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.ShowConstraints where

import Agda.Interaction.BasicOps (getConstraints)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Monad.Base (TCM)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List

showConstraints :: [ByteString] -> TCM ByteString
showConstraints = \case
  [] -> constraints
  [""] -> constraints
  _  -> pure "/agda constraints [cid]"
  where
    constraintToBS = BS8.pack . unlines . List.map prettyShow
    constraints = getConstraints >>= pure . constraintToBS

