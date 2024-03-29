{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.Reload where

import Agda.Interaction.Imports
  ( CheckResult, Mode (..), crInterface, crMode, crWarnings
  , typeCheckMain, parseSource
  )
import Agda.Interaction.FindFile (SourceFile (..))
import Agda.Interaction.Options (optOnlyScopeChecking)
import Agda.Syntax.Translation.ConcreteToAbstract (importPrimitives)
import Agda.TypeChecking.Errors (applyFlagsToTCWarnings, renderError)
import Agda.TypeChecking.Monad.Base
  (ModuleCheckMode (..), TypeError (..), TCM, commandLineOptions, iInsideScope, typeError)
import Agda.TypeChecking.Monad.State (setScope)
import Agda.Utils.FileName (AbsolutePath)
import Agda.Utils.Null (unlessNullM)
import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)

import Proof.Assistant.Helpers (toBS)

reload :: Maybe AbsolutePath -> TCM ByteString
reload Nothing = pure "Failed to type check."
reload (Just file) = do
  checked <- checkFile file
  setScope $ iInsideScope (crInterface checked)
  _ <- importPrimitives
  pure "Type checked succesfully."
  `catchError` \e -> do
    s <- renderError e
    pure $ "Failed. " <> toBS s

checkFile :: AbsolutePath -> TCM CheckResult
checkFile inputFile = do
  opts <- commandLineOptions
  let mode =
        if optOnlyScopeChecking opts
          then ScopeCheck
          else TypeCheck

  result <- typeCheckMain mode =<< parseSource (SourceFile inputFile)

  unless (crMode result == ModuleScopeChecked) $
    unlessNullM (applyFlagsToTCWarnings (crWarnings result)) $ \ ws ->
      typeError $ NonFatalErrors ws

  pure result
