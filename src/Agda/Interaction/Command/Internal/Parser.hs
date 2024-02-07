module Agda.Interaction.Command.Internal.Parser where

import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Pretty (text)
import Agda.Syntax.Parser (exprParser, parse, parsePosString)
import Agda.Syntax.Position (getRange, noRange, rStart)
import Agda.Syntax.Translation.ConcreteToAbstract
  (checkAttributes, concreteToAbstract, localToAbstract)
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.MetaVars (lookupInteractionId, lookupLocalMeta, withInteractionId)
import Agda.TypeChecking.Warnings (runPM)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Control.Monad.Except (MonadError(..))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as BS8

metaParseExpr ::  InteractionId -> ByteString -> TCM Expr
metaParseExpr ii s =
    do  m <- lookupInteractionId ii
        scope <- getMetaScope <$> lookupLocalMeta m
        r <- getRange <$> lookupLocalMeta m
        -- liftIO $ putStrLn $ prettyShow scope
        let pos = fromMaybe (__IMPOSSIBLE__) (rStart r)
        (e, coh) <- runPM $ parsePosString exprParser pos (BS8.unpack s)
        checkAttributes coh
        concreteToAbstract scope e

actOnMeta :: [ByteString] -> (InteractionId -> Expr -> TCM a) -> TCM a
actOnMeta (is:es) f =
     do  i <- readM (BS8.unpack is)
         let ii = InteractionId i
         e <- metaParseExpr ii (BS8.unwords es)
         withInteractionId ii $ f ii e
actOnMeta _ _ = __IMPOSSIBLE__

parseExpr :: String -> TCM Expr
parseExpr s = do
    (e, coh) <- runPM $ parse exprParser s
    checkAttributes coh
    localToAbstract e return

readM :: Read a => String -> TCM a
readM s = maybe err return $ readMaybe s
  where
  err    = throwError $ strMsg $ "Cannot parse: " ++ s
  strMsg = Exception noRange . text
