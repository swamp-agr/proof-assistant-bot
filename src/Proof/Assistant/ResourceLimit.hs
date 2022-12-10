{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.ResourceLimit where

import GHC.Natural
import System.Posix.Process (nice)
import System.Posix.Resource

import Proof.Assistant.Helpers
import Proof.Assistant.Settings

-- | Make ResourceLimits from soft/hard limits.
makeResourceLimits :: Limit -> ResourceLimits
makeResourceLimits (Limit soft hard)
  = ResourceLimits (toResourceLimit soft) (toResourceLimit hard)
  where
    toResourceLimit = ResourceLimit . fromIntegral

-- | Read Resources from 'ResourceSettings'.
makeLimits :: ResourceSettings -> [(Resource, ResourceLimits)]
makeLimits ResourceSettings{..} =
  [ (ResourceTotalMemory,  makeResourceLimits totalMemory)
  , (ResourceOpenFiles,    makeResourceLimits openFiles)
  , (ResourceFileSize,     makeResourceLimits fileSize)
  , (ResourceDataSize,     makeResourceLimits dataSize)
  , (ResourceCPUTime,      makeResourceLimits cpuTime)
  ]

-- | Read limits from 'ResourceSettings' and set them for current thread.
setLimits :: ResourceSettings -> IO ()
setLimits = mapM_ (uncurry setResourceLimit) . makeLimits

-- | Set 'Priority' for current thread.
setPriority :: Priority -> IO ()
setPriority = nice . toInt @_ @Natural
