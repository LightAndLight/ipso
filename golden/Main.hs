{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Selective (whenS)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Dhall
import Options.Applicative (Parser, execParser, fullDesc, help, info, long, metavar, strOption)
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
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

runExample :: Config -> Example -> IO ()
runExample config example = do
  let args = foldr ((:) . Text.unpack) [] $ exArgs example
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      (cfgBin config)
      args
      ""

  hasErrorRef <- newIORef False

  unless (stdout == Text.unpack (exStdout example)) $ do
    modifyIORef hasErrorRef (True ||)
    displayError "stdout" (exPath example) (exStdout example, id) (stdout, id)

  unless (stderr == Text.unpack (exStderr example)) $ do
    modifyIORef hasErrorRef (True ||)
    displayError "stderr" (exPath example) (exStderr example, id) (stderr, id)

  unless (eqExitCode (exExitCode example) exitCode) $ do
    modifyIORef hasErrorRef (True ||)
    displayError "exitcode" (exPath example) (exExitCode example, Text.pack . show) (exitCode, show)

  whenS (readIORef hasErrorRef) exitFailure
 where
  displayError label path (expected, showExpected) (actual, showActual) = do
    putStrLn $ path <> ": " <> label <> " mismatch"
    putStrLn "expected:"
    Text.putStrLn $ showExpected expected
    putStrLn "actual:"
    putStrLn $ showActual actual

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
  for_ files $ \file -> do
    example <- Dhall.inputFile (exampleDecoder file) file
    runExample config example
  putStrLn "golden tests passed\n"
