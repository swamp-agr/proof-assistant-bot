{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Proof.Assistant.Helpers where

import Data.Coerce (Coercible, coerce)
import Data.Text (Text)

import qualified Data.Text as Text

toInt :: forall a b. (Num b, Integral b, Coercible a b) => a -> Int
toInt = fromIntegral . coerce @_ @b


t2s :: Coercible a Text => a -> String
t2s = Text.unpack . coerce
