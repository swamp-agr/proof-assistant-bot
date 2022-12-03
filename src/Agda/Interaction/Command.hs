{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Agda.Interaction.Command where

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import System.FilePath

import Agda.Interaction.Command.EvalIn
import Agda.Interaction.Command.EvalTerm
import Agda.Interaction.Command.GiveMeta
import Agda.Interaction.Command.RefineMeta
import Agda.Interaction.Command.Reload
import Agda.Interaction.Command.RetryConstraints
import Agda.Interaction.Command.ShowConstraints
import Agda.Interaction.Command.ShowContext
import Agda.Interaction.Command.ShowMetas
import Agda.Interaction.Command.ShowScope
import Agda.Interaction.Command.TypeOf
import Agda.Interaction.Command.TypeIn

import Agda.Interaction.State
import Agda.TypeChecking.Monad.Base (TCEnv (..), TCM)
import Agda.Utils.FileName (AbsolutePath, filePath)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap

data AgdaCommand
  = Reload | Help | Constraints | Context | Give | Refine | Meta | Load
  | Eval | TypeOf | TypeIn | WakeUp | Scope
  deriving (Eq, Show, Generic)

supportedCommands :: HashMap ByteString AgdaCommand
supportedCommands = HashMap.fromList
  [ (,) "/reload" Reload
  , (,) "/?" Help
  , (,) "/constraints" Constraints
  , (,) "/context" Context
  , (,) "/give" Give
  , (,) "/refine" Refine
  , (,) "/meta" Meta
  , (,) "/load" Load
  , (,) "/eval" Eval
  , (,) "/typeOf" TypeOf
  , (,) "/typeIn" TypeIn
  , (,) "/wakeup" WakeUp
  , (,) "/scope" Scope
  ]
  
matchSupported :: ByteString -> Maybe AgdaCommand
matchSupported = (`HashMap.lookup` supportedCommands)

chooseCommand :: AgdaState -> Either () AgdaCommand -> ByteString -> IO (TCM ByteString)
chooseCommand _state (Left _) input = pure (evalTerm input)
chooseCommand state (Right cmd) input = do
  mfilePath <- envCurrentPath <$> readTCEnv state
  case cmd of
    Constraints -> pure (showConstraints (BS8.words input))
    Context     -> pure (showContext (BS8.words input))
    Give        -> pure (giveMeta (BS8.words input))
    Refine      -> pure (refineMeta (BS8.words input))
    Scope       -> pure showScope
    Meta        -> pure (showMetas (BS8.words input))
    Load        -> case mfilePath of
      Nothing   -> pure (pure "Failed to load.")
      Just filepath -> do
        storeRequestContent filepath input
        pure $ reload (Just filepath)
    Reload      -> pure (reload mfilePath)
    WakeUp      -> pure retryConstraints
    TypeOf      -> pure (typeOf (BS8.words input))
    TypeIn      -> pure (typeIn (BS8.words input))
    Eval        -> pure (evalIn (BS8.words input))
    _           -> pure (pure tbd)
  where
    tbd = BS8.unwords
      [ "Command"
      , BS8.pack (show cmd)
      , "is not supported yet."
      ]

storeRequestContent :: AbsolutePath -> ByteString -> IO ()
storeRequestContent absFilepath content = do
  let moduleName = (BS8.pack . dropExtension . takeFileName . filePath) absFilepath
      moduleContent = BS8.unlines
        [ BS8.unwords [ "module", moduleName, "where" ]
        , ""
        , content
        ]
  BS8.writeFile (filePath absFilepath) moduleContent
