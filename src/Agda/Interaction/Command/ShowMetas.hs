{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Agda.Interaction.Command.ShowMetas where

import Agda.Interaction.Base (OutputConstraint (..),  OutputForm (..), Rewrite (..))
import Agda.Interaction.BasicOps
  (outputFormId, typeOfMeta, typesOfHiddenMetas, typesOfVisibleMetas)
import Agda.Syntax.Abstract.Pretty (prettyA)
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Position (noRange)
import Agda.TypeChecking.Monad.Base (TCM, nmid)
import Agda.TypeChecking.Monad.MetaVars
  (getInteractionRange, getMetaRange, withInteractionId, withMetaId)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Pretty (prettyShow, render)
import Data.ByteString (ByteString)

import Agda.Interaction.Command.Internal.Parser

import qualified Agda.Syntax.Internal as I
import qualified Data.ByteString.Char8 as BS8

showMetas :: [ByteString] -> TCM ByteString
showMetas [m] =
    do  i <- InteractionId <$> readM (BS8.unpack m)
        withInteractionId i $ do
          s <- typeOfMeta AsIs i
          r <- getInteractionRange i
          d <- prettyA s
          pure $ toBS $ render d ++ " " ++ prettyShow r
showMetas [m,"normal"] =
    do  i <- InteractionId <$> readM (BS8.unpack m)
        withInteractionId i $ do
          s <- prettyA =<< typeOfMeta Normalised i
          r <- getInteractionRange i
          pure $ toBS $ render s ++ " " ++ prettyShow r
showMetas [] =
    do  interactionMetas <- typesOfVisibleMetas AsIs
        hiddenMetas      <- typesOfHiddenMetas  AsIs
        shownInteractionMetas <- mapM printII interactionMetas
        shownHiddenMetas <- mapM printM hiddenMetas
        pure $ BS8.unlines
          [ "Interaction metas: " <> BS8.unwords shownInteractionMetas
          , ""
          , "Hidden metas: " <> BS8.unwords shownHiddenMetas
          ]
    where
        showII o = withInteractionId (outputFormId $ OutputForm noRange [] I.alwaysUnblock o) $ prettyA o
        showM  o = withMetaId (nmid $ outputFormId $ OutputForm noRange [] I.alwaysUnblock o) $ prettyA o

        metaId (OfType i _) = i
        metaId (JustType i) = i
        metaId (JustSort i) = i
        metaId (Assign i _e) = i
        metaId _ = __IMPOSSIBLE__
        printM x = do
            r <- getMetaRange $ nmid $ metaId x
            d <- showM x
            pure $ toBS $ render d ++ "  [ at " ++ prettyShow r ++ " ]"
        printII x = do
          d <- showII x
          pure $ toBS $ render d
showMetas _ = pure ":meta [metaid]"

