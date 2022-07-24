{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Ipso.Common (Config(..), displayError, examplesMain, eqExitCode, runDiff)
import Control.Applicative (optional)
import Control.Monad (unless)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Dhall
import Options.Applicative (Parser, help, long, metavar, strOption)
import System.Process (readProcessWithExitCode)

data Example = Example
  { exPath :: FilePath
  , exDescription :: Text
  , exArgs :: Vector Text
  , exStdin :: Maybe Text
  , exStdout :: Text
  , exStderr :: Text
  , exExitCode :: Dhall.Natural
  }

exampleDecoder :: FilePath -> Dhall.Decoder Example
exampleDecoder path =
  Dhall.record $
    Example path
      <$> Dhall.field "description" Dhall.strictText
        <*> Dhall.field "args" (Dhall.vector Dhall.strictText)
        <*> Dhall.field "stdin" (Dhall.maybe Dhall.strictText)
        <*> Dhall.field "stdout" Dhall.strictText
        <*> Dhall.field "stderr" Dhall.strictText
        <*> Dhall.field "exitcode" Dhall.natural

runExample :: Config (Maybe FilePath) -> Example -> IO (Either String ())
runExample config example = do
  let args = foldr ((:) . Text.unpack) [] $ exArgs example
  let ipsoPath = fromMaybe "ipso" (cfgRest config)
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      ipsoPath
      args
      (maybe "" Text.unpack $ exStdin example)

  errorRef <- newIORef Nothing

  mStdoutDiff <- runDiff (exStdout example) (Text.pack stdout)
  for_ mStdoutDiff $ \diff ->
    modifyIORef errorRef (<> pure (
      unlines
        [ exPath example <> ": stdout mismatch"
        , diff
        ]
    ))

  mStderrDiff <- runDiff (exStderr example) (Text.pack stderr)
  for_ mStderrDiff $ \diff ->
    modifyIORef errorRef (<> pure (
      unlines
        [ exPath example <> ": stderr mismatch"
        , diff
        ]
    ))

  unless (eqExitCode (exExitCode example) exitCode) $ do
    modifyIORef errorRef (<> pure (displayError "exitcode" (exPath example) (exExitCode example, Text.pack . show) (exitCode, show)))

  mError <- readIORef errorRef
  pure $
    case mError of
      Just err -> Left err
      Nothing -> Right ()

binParser :: Parser (Maybe FilePath)
binParser =
  optional
    (strOption
      ( long "bin"
          <> help "path to binary"
          <> metavar "PATH"
      ))

main :: IO ()
main = examplesMain binParser exampleDecoder runExample
