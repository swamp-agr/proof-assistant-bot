{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Settings where

import Dhall
import Data.HashMap.Strict (HashMap)

data Settings = Settings
  { botName :: !Text
  , allowedCommands :: ![Text]
  , botToken :: !Text
  , outputSize :: !Natural
  , help :: Text
  , helpMessages :: HashMap Text Text
  , version :: Text
  , interpretersSettings :: !InterpretersSettings
  } deriving (Generic, FromDhall)

data ExternalInterpreterSettings = ExternalInterpreterSettings
  { args :: !CmdArgs
  , packages :: !Packages
  , executable :: !Executable
  , time :: !Time -- ^ Time, in seconds.
  , priority :: !Priority
  , resources :: !ResourceSettings
  , tempFilePrefix :: !FilePath
  , fileExtension :: !FilePath
  , inputSize :: !Natural
  } deriving (Generic, FromDhall)

data InternalInterpreterSettings = InternalInterpreterSettings
  { timeout :: !Natural
  , allocations :: !Natural
  , inputSize :: !Natural
  , sourceFilePrefix :: !FilePath
  , sourceFileExtension :: !FilePath
  } deriving (Generic, FromDhall)

data AgdaSettings = AgdaSettings
  { internal :: !InternalInterpreterSettings
  } deriving (Generic, FromDhall)

data LeanSettings = LeanSettings
  { projectDir :: !FilePath
  , externalLean   :: !ExternalInterpreterSettings
  } deriving (Generic, FromDhall)

data ArendSettings = ArendSettings
  { arendRootProjectDir :: !FilePath
  , arendYamlFilename :: !FilePath
  , arendYamlContent :: !Text
  , arendModuleName :: !FilePath
  , externalArend :: !ExternalInterpreterSettings
  } deriving (Generic, FromDhall)

newtype IdrisSettings = IdrisSettings ExternalInterpreterSettings
  deriving newtype (FromDhall, ToInterpreterState)
  deriving stock Generic

newtype CmdArgs = CmdArgs [Text]
  deriving stock Generic
  deriving newtype FromDhall

newtype Packages = Packages Text
  deriving stock Generic
  deriving newtype FromDhall

newtype Executable = Executable Text
  deriving stock Generic
  deriving newtype FromDhall

newtype Time = Time Natural
  deriving stock Generic
  deriving newtype FromDhall

newtype Priority = Priority Natural
  deriving stock Generic
  deriving newtype FromDhall

data ResourceSettings = ResourceSettings
  { totalMemory :: !Limit
  , dataSize    :: !Limit
  , openFiles   :: !Limit
  , fileSize    :: !Limit
  , cpuTime     :: !Limit
  } deriving (Generic, FromDhall)

data Limit = Limit
  { soft :: !Natural
  , hard :: !Natural
  } deriving (Generic, FromDhall)

data InterpretersSettings = InterpretersSettings
  { agda  :: !AgdaSettings
  , arend :: !ArendSettings
  , idris :: !IdrisSettings
  , coq   :: !ExternalInterpreterSettings
  , lean  :: !LeanSettings
  , rzk   :: !InternalInterpreterSettings
  } deriving (Generic, FromDhall)

loadSettings :: Text -> IO Settings
loadSettings = input auto

loadDefaultSettings :: IO Settings
loadDefaultSettings = loadSettings "./config/settings.dhall"

class ToInterpreterState settings where
  getQueueSize :: settings -> Natural

instance ToInterpreterState InternalInterpreterSettings where
  getQueueSize = inputSize

instance ToInterpreterState ExternalInterpreterSettings where
  getQueueSize = inputSize

instance ToInterpreterState AgdaSettings where
  getQueueSize = getQueueSize . internal

instance ToInterpreterState LeanSettings where
  getQueueSize = getQueueSize . externalLean

instance ToInterpreterState ArendSettings where
  getQueueSize = getQueueSize . externalArend
