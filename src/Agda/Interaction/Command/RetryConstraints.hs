{-# LANGUAGE OverloadedStrings #-}
module Agda.Interaction.Command.RetryConstraints where

import Agda.TypeChecking.Constraints (wakeupConstraints_)
import Agda.TypeChecking.Monad.Base (TCM)
import Data.ByteString (ByteString)

retryConstraints :: TCM ByteString
retryConstraints = wakeupConstraints_ >> pure "Awaken."
