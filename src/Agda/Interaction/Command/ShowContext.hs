{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.ShowContext where

import Agda.Interaction.Command.Internal.Parser
import Agda.Syntax.Common (InteractionId (..), argNameToString)
import Agda.Syntax.Common.Pretty ((<+>), render, text)
import Agda.TypeChecking.Monad.Base (TCM, getMetaInfo)
import Agda.TypeChecking.Monad.Context (getContextTelescope)
import Agda.TypeChecking.Monad.MetaVars (lookupInteractionId, lookupLocalMeta, withMetaInfo)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Reduce (normalise)
import Agda.TypeChecking.Substitute.Class (raise)
import Control.Monad (zipWithM)
import Data.ByteString (ByteString)

import qualified Agda.Syntax.Internal as I
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List

showContext :: [ByteString] -> TCM ByteString
showContext (meta:args) = do
    i <- InteractionId <$> readM (BS8.unpack meta)
    mi <- lookupLocalMeta =<< lookupInteractionId i
    BS8.unlines <$> withMetaInfo (getMetaInfo mi) displayMore
  where
    display (x, t) n = do
      t0 <- case args of
             ["normal"] -> normalise $ raise n t
             _          -> return $ raise n t
      d <- prettyTCM t0
      pure $ BS8.pack $ render $ text (argNameToString x) <+> ":" <+> d
    displayMore = do
      ctx <- List.map I.unDom . I.telToList <$> getContextTelescope
      zipWithM display ctx $ reverse $ zipWith const [1..] ctx
showContext _ = pure "context meta"

