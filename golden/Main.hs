{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Semigroup (Option (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Dhall
import Options.Applicative (Parser, execParser, fullDesc, help, info, long, metavar, strOption)
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeExtension, (</>))
import System.Process (readProcessWithExitCode)

data Example = Example
  { exPath :: FilePath
  , exArgs :: Vector Text
  , exStdout :: Text
  , exStderr :: Text
  , exExitCode :: Integer
  }

exampleDecoder :: FilePath -> Dhall.Decoder Example
exampleDecoder path =
  Dhall.record $
    Example path
      <$> Dhall.field "args" (Dhall.vector Dhall.strictText)
        <*> Dhall.field "stdout" Dhall.strictText
        <*> Dhall.field "stderr" Dhall.strictText
        <*> Dhall.field "exitcode" Dhall.integer

eqExitCode :: Integer -> ExitCode -> Bool
eqExitCode e1 e2 =
  case e1 of
    0 ->
      case e2 of
        ExitSuccess -> True
        ExitFailure 0 -> True
        _ -> False
    n1 ->
      case e2 of
        ExitFailure n2 -> n1 == fromIntegral n2
        _ -> False

runExample :: Config -> Example -> IO (Either String ())
runExample config example = do
  let args = foldr ((:) . Text.unpack) [] $ exArgs example
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      (cfgBin config)
      args
      ""

  errorRef <- newIORef $ Option Nothing

  unless (stdout == Text.unpack (exStdout example)) $ do
    modifyIORef errorRef (<> pure (displayError "stdout" (exPath example) (exStdout example, id) (stdout, id)))

  unless (stderr == Text.unpack (exStderr example)) $ do
    modifyIORef errorRef (<> pure (displayError "stderr" (exPath example) (exStderr example, id) (stderr, id)))

  unless (eqExitCode (exExitCode example) exitCode) $ do
    modifyIORef errorRef (<> pure (displayError "exitcode" (exPath example) (exExitCode example, Text.pack . show) (exitCode, show)))

  Option mError <- readIORef errorRef
  pure $
    case mError of
      Just err -> Left err
      Nothing -> Right ()
 where
  displayError label path (expected, showExpected) (actual, showActual) =
    unlines
      [ path <> ": " <> label <> " mismatch"
      , "expected:"
      , Text.unpack $ showExpected expected
      , "actual:"
      , showActual actual
      ]

data Config = Config
  { cfgBin :: FilePath
  , cfgDir :: FilePath
  }

configParser :: FilePath -> Parser Config
configParser curDir =
  (\bin dir -> Config (curDir </> bin) (curDir </> dir))
    <$> strOption
      ( long "bin"
          <> help "path to binary"
          <> metavar "PATH"
      )
    <*> strOption
      ( long "dir"
          <> help "location of golden tests"
          <> metavar "DIRECTORY"
      )

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  config <- execParser (info (configParser curDir) fullDesc)
  setCurrentDirectory $ cfgDir config
  files <- listDirectory "."
  results <- for (filter ((".dhall" ==) . takeExtension) files) $ \file -> do
    example <- Dhall.inputFile (exampleDecoder file) file
    runExample config example
  let (failures, successes) = Either.partitionEithers results
  putStrLn $ show (length successes) <> " examples passed"
  putStrLn $ show (length failures) <> " examples failed"
  traverse_ putStrLn failures
  case failures of
    [] -> exitSuccess
    _ : _ -> exitFailure
