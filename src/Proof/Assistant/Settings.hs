{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Proof.Assistant.Settings where

import Dhall
import Data.HashMap.Strict (HashMap)

data Settings = Settings
  { botName :: !Text -- ^ Telegram bot name. Used to parse @/command\@botname@.
  , botToken :: !Text -- ^ Bot token.
  , outputSize :: !Natural -- ^ output size in bytes.
  , help :: Text -- ^ Help message.
  , helpMessages :: HashMap Text Text -- ^ Help messages for sub-commands.
  , version :: Text -- ^ Version message.
  , interpretersSettings :: !InterpretersSettings -- ^ Settings for backend interpreters.
  , sharedDir :: !FilePath
  } deriving (Generic, FromDhall)

data ExternalInterpreterSettings = ExternalInterpreterSettings
  { args :: !CmdArgs -- ^ CLI arguments.
  , executable :: !Executable -- ^ Path to executable.
  , time :: !Time -- ^ Time, in seconds.
  , priority :: !Priority -- ^ Priority for the thread.
  , resources :: !ResourceSettings -- ^ Different resources.
  , tempFilePrefix :: !FilePath -- ^ Prefix to filepath to avoid confusion between different interpreters.
  , fileExtension :: !FilePath -- ^ File extension used by interpreter.
  , inputSize :: !Natural -- ^ input size in bytes.
  , sandbox :: !(Maybe SandboxSettings)
  } deriving (Generic, FromDhall)

data InternalInterpreterSettings = InternalInterpreterSettings
  { timeout :: !Natural -- ^ Timeout in seconds.
  , allocations :: !Natural -- ^ Max number of GHC allocations for the interpreter thread.
  , inputSize :: !Natural -- ^ input size in bytes.
  , sourceFilePrefix :: !FilePath -- ^ Prefix to filepath to avoid confusion between different interpreters.
  , sourceFileExtension :: !FilePath -- ^ File extension used by interpreter.
  } deriving (Generic, FromDhall)

data AgdaSettings = AgdaSettings
  { internal :: !InternalInterpreterSettings
  , agdaSrcDir :: !FilePath
  , agdaStdlibDir :: !FilePath
  } deriving (Generic, FromDhall)

data LeanSettings = LeanSettings
  { projectDir :: !FilePath -- ^ Lean requires project where to store source files for typechecking.
  , externalLean   :: !ExternalInterpreterSettings
  } deriving (Generic, FromDhall)

data ArendSettings = ArendSettings
  { arendRootProjectDir :: !FilePath -- ^ Arend requires different directories to work correctly. One of them used to store libraries, settings, user projects. We call it "root directory".
  , arendYamlFilename :: !FilePath -- ^ For each project Arend requires yaml file with a specific name.
  , arendYamlContent :: !Text -- ^ For each project Arend requires yaml file to store specific settings inside.
  , arendModuleName :: !FilePath -- ^ Since Arend requires project per chat, it will have a single module inside and we want to give it a common specific configurable name.
  , externalArend :: !ExternalInterpreterSettings
  } deriving (Generic, FromDhall)

data AlloySettings = AlloySettings
  { alloyProjectDir :: !FilePath
  , dotGraphExecutable :: !Executable
  , dotGraphArgs :: !CmdArgs
  , imageHelperExecutable :: !Executable
  , imageHelperArgs :: !CmdArgs
  , gifConverterExecutable :: !Executable
  , gifConverterArgs :: !CmdArgs
  , alloySharedDir :: !FilePath
  , externalAlloy :: !ExternalInterpreterSettings
  } deriving (Generic, FromDhall)

newtype IdrisSettings = IdrisSettings ExternalInterpreterSettings
  deriving newtype (FromDhall, ToInterpreterState)
  deriving stock Generic

newtype CmdArgs = CmdArgs [Text]
  deriving stock Generic
  deriving newtype (FromDhall, Semigroup)

newtype Executable = Executable Text
  deriving stock Generic
  deriving newtype FromDhall

newtype Time = Time Natural
  deriving stock Generic
  deriving newtype FromDhall

newtype Priority = Priority Natural
  deriving stock Generic
  deriving newtype FromDhall

-- | Resource settings.
data ResourceSettings = ResourceSettings
  { totalMemory :: !Limit
  , dataSize    :: !Limit
  , openFiles   :: !Limit
  , fileSize    :: !Limit
  , cpuTime     :: !Limit
  } deriving (Generic, FromDhall)

-- | Limits.
data Limit = Limit
  { soft :: !Natural
  , hard :: !Natural
  } deriving (Generic, FromDhall)

data SandboxSettings = SandboxSettings
  { sandboxExecutable :: !Executable
  , sandboxArgs :: !CmdArgs
  } deriving (Generic, FromDhall)

-- | Combination of all supported interpreters settings.
data InterpretersSettings = InterpretersSettings
  { agda  :: !AgdaSettings
  , arend :: !ArendSettings
  , idris :: !IdrisSettings
  , coq   :: !ExternalInterpreterSettings
  , lean  :: !LeanSettings
  , rzk   :: !InternalInterpreterSettings
  , alloy :: !AlloySettings
  } deriving (Generic, FromDhall)

-- | Load settings from file.
loadSettings :: Text -> IO Settings
loadSettings = input auto

-- | Load default settings.
loadDefaultSettings :: IO Settings
loadDefaultSettings = loadSettings "./config/settings.dhall"

-- | Helper to get queue size (communication model between bot and backend worker).
class ToInterpreterState settings where
  getQueueSize :: settings -> Natural

instance ToInterpreterState InternalInterpreterSettings where
  getQueueSize InternalInterpreterSettings{inputSize} = inputSize

instance ToInterpreterState ExternalInterpreterSettings where
  getQueueSize ExternalInterpreterSettings{inputSize} = inputSize

instance ToInterpreterState AgdaSettings where
  getQueueSize = getQueueSize . internal

instance ToInterpreterState LeanSettings where
  getQueueSize = getQueueSize . externalLean

instance ToInterpreterState ArendSettings where
  getQueueSize = getQueueSize . externalArend

instance ToInterpreterState AlloySettings where
  getQueueSize = getQueueSize . externalAlloy
