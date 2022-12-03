{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.ResourceLimit where

import GHC.Natural
import System.Posix.Process (nice)
import System.Posix.Resource

import Proof.Assistant.Helpers
import Proof.Assistant.Settings

makeResourceLimits :: Limit -> ResourceLimits
makeResourceLimits (Limit soft hard)
  = ResourceLimits (toResourceLimit soft) (toResourceLimit hard)
  where
    toResourceLimit = ResourceLimit . fromIntegral

makeLimits :: ResourceSettings -> [(Resource, ResourceLimits)]
makeLimits ResourceSettings{..} =
  [ (ResourceTotalMemory,  makeResourceLimits totalMemory)
  , (ResourceOpenFiles,    makeResourceLimits openFiles)
  , (ResourceFileSize,     makeResourceLimits fileSize)
  , (ResourceDataSize,     makeResourceLimits dataSize)
  , (ResourceCPUTime,      makeResourceLimits cpuTime)
  ]

setLimits :: ResourceSettings -> IO ()
setLimits = mapM_ (uncurry setResourceLimit) . makeLimits

setPriority :: Priority -> IO ()
setPriority = nice . toInt @_ @Natural
