{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Settings where

import Dhall

data Settings = Settings
  { botName :: !Text
  , allowedCommands :: ![Text]
  , botToken :: !Text
  , outputSize :: !Natural
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

newtype CmdArgs = CmdArgs Text
  deriving (Generic, FromDhall)

newtype Packages = Packages Text
  deriving (Generic, FromDhall)  

newtype Executable = Executable Text
  deriving (Generic, FromDhall)

newtype Time = Time Natural
  deriving (Generic, FromDhall)

newtype Priority = Priority Natural
  deriving (Generic, FromDhall)

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
  , arend :: !ExternalInterpreterSettings
  , idris :: !ExternalInterpreterSettings
  , coq   :: !ExternalInterpreterSettings
  , lean  :: !ExternalInterpreterSettings
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
