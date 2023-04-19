{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Alloy where

import Control.Concurrent.Async (race)
import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.Maybe (mapMaybe)
import Data.Text (unpack, pack)
import System.Directory
  (createDirectoryIfMissing, listDirectory, removeFile, withCurrentDirectory)
import System.Exit (ExitCode)
import System.FilePath
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import Proof.Assistant.Helpers
import Proof.Assistant.RefreshFile
import Proof.Assistant.Request
import Proof.Assistant.Response
import Proof.Assistant.ResourceLimit
import Proof.Assistant.Settings
import Proof.Assistant.State

callAlloy :: InterpreterState AlloySettings -> InterpreterRequest -> IO BotResponse
callAlloy InterpreterState{..} ir = do
  let AlloySettings{..} = coerce settings
      s@ExternalInterpreterSettings{..} = externalAlloy
      projectDir = alloyProjectDir
        </> (tempFilePrefix  <> chatIdToString (interpreterRequestTelegramChatId ir))
  createDirectoryIfMissing True projectDir
  (dir, path) <- refreshTmpFile s ir (Just projectDir)
  withCurrentDirectory dir $ do
    let run
          :: Executable -> CmdArgs -> FilePath -> Maybe [FilePath]
          -> IO (ExitCode, String, String)
        run exe arguments path' mPath = do
          readProcessWithExitCode (t2s exe) fullArgs ""
          where
            addPathAsArg "" = id
            addPathAsArg p = (<> [p])

            applyOutMaybe = case mPath of
              Nothing -> id
              Just extra -> (<> extra)
            fullArgs = applyOutMaybe $ addPathAsArg path' (unpack <$> coerce arguments)
        runProcess = run executable args path Nothing
        asyncExecutable = do
          setPriority priority
          (_exitCode, stdout, stderr) <- runProcess
          pure . validate path . toBS . unlines $ [stdout, stderr]
        asyncTimer = asyncWait time

        mkImagePath external file =
          let changeAbsolute = if external
                then (alloySharedDir </>) . takeFileName else id
          in changeAbsolute ((dropExtension file) <.> "png")
        mkGifPath file = (alloySharedDir </>) . takeFileName $ (dropExtension file) <.> "gif"

    eresult <- race asyncTimer (handleErrorMaybe asyncExecutable)
    result <- case eresult of
      Left ()  -> pure (TextResponse "Time limit exceeded")
      Right bs -> findDots dir >>= \case
        [] -> return (TextResponse bs)
        dot : [] -> do
          let imagePath = mkImagePath True dot
          _ <- run dotGraphExecutable dotGraphArgs dot $ Just ["-o" <> imagePath]
          pure (ImageResponse "image/png" imagePath Nothing Nothing)
        dots -> do
          images <- forM dots $ \dot -> do
            let imagePath = mkImagePath False dot
            _ <- run dotGraphExecutable dotGraphArgs dot $ Just ["-o" <> imagePath]
            pure imagePath

          let imagesAsArgs = CmdArgs (pack <$> images)
              fullImgHelperArgs = imageHelperArgs <> imagesAsArgs

          (_code, stdout, _stderr) <- run imageHelperExecutable fullImgHelperArgs "" Nothing

          let (mwidth, mheight, canvasArgs) = makeCanvasArgs stdout
              fullGifArgs = canvasArgs <> gifConverterArgs <> imagesAsArgs
              gifPath = mkGifPath path

          _ <- run gifConverterExecutable fullGifArgs gifPath Nothing
          pure (ImageResponse "image/gif" gifPath mwidth mheight)
    removeIntermediateFiles dir
    return result

makeCanvasArgs :: String -> (Maybe Int, Maybe Int, CmdArgs)
makeCanvasArgs stdout = (mwidth, mheight, args) 
  where
    splitOn c x = (takeWhile (/= c) x, reverse . takeWhile (/= c) . reverse $ x)
    parse = fmap (splitOn '-') . filter (/= mempty) . lines
    parseInts (w, h) = case (readMaybe w, readMaybe h) of
      (Just wn, Just hn) -> Just (wn, hn)
      (Just wn, Nothing) -> Just (wn, 0 :: Int)
      (Nothing, Just hn) -> Just (0 :: Int, hn)
      _ -> Nothing
    safeMaximum [] = 0
    safeMaximum xs = maximum xs
    textDimensions = parse stdout
    dimensions = mapMaybe parseInts textDimensions
    maxWidth = safeMaximum (fst <$> dimensions)
    maxHeight = safeMaximum (snd <$> dimensions)
    restoreMaybe 0 = Nothing
    restoreMaybe x = Just x

    mwidth = restoreMaybe maxWidth
    mheight = restoreMaybe maxHeight
    dimensionsAsArgs = s2t maxWidth <> "x" <> s2t maxHeight
    args = CmdArgs ["-size", dimensionsAsArgs, "-extent", dimensionsAsArgs, "-sample" dimensionsAsArgs]

findDots :: FilePath -> IO [FilePath]
findDots dir = do
  contents <- listDirectory dir
  pure . filter ((== ".dot") . takeExtension) $ contents

removeIntermediateFiles :: FilePath -> IO ()
removeIntermediateFiles dir
  = listDirectory dir >>= mapM_ removeFile . filter ((/= ".als") . takeExtension)
