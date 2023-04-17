{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Proof.Assistant.Alloy where

import Control.Concurrent.Async (race)
import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.Text (unpack, pack)
import System.Directory
  (createDirectoryIfMissing, listDirectory, removeFile, withCurrentDirectory)
import System.Exit (ExitCode)
import System.FilePath
import System.Process (readProcessWithExitCode)

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
            applyOutMaybe = case mPath of
              Nothing -> id
              Just extra -> (<> extra)
            fullArgs = applyOutMaybe ((unpack <$> coerce arguments) <> [path'])
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
          pure (ImageResponse "image/png" imagePath)
        dots -> do
          images <- forM dots $ \dot -> do
            let imagePath = mkImagePath False dot
            _ <- run dotGraphExecutable dotGraphArgs dot $ Just ["-o" <> imagePath]
            pure imagePath
          let fullGifArgs = gifConverterArgs <> CmdArgs (pack <$> images)
              gifPath = mkGifPath path
          _ <- run gifConverterExecutable fullGifArgs gifPath Nothing
          pure (ImageResponse "image/gif" gifPath)
    removeIntermediateFiles dir
    return result

findDots :: FilePath -> IO [FilePath]
findDots dir = do
  contents <- listDirectory dir
  pure . filter ((== ".dot") . takeExtension) $ contents

removeIntermediateFiles :: FilePath -> IO ()
removeIntermediateFiles dir
  = listDirectory dir >>= mapM_ removeFile . filter ((/= ".als") . takeExtension)
