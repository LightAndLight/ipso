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
import Options.Applicative (Parser, ReadM, execParser, fullDesc, help, info, long, metavar, option, str, strOption, value)
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeExtension, (</>))
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

eqExitCode :: (Eq a, Num a) => a -> ExitCode -> Bool
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
      (maybe "" Text.unpack $ exStdin example)

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
  , cfgOnly :: Maybe [String]
  }

commasReader :: ReadM [String]
commasReader = go <$> str
 where
  go :: String -> [String]
  go input =
    let (prefix, suffix) = break (== ',') input
     in prefix :
        case suffix of
          [] -> []
          _ : rest ->
            go rest

configParser :: FilePath -> Parser Config
configParser curDir =
  (\bin dir only -> Config (curDir </> bin) (curDir </> dir) only)
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
    <*> option
      (Just <$> commasReader)
      ( long "only"
          <> help "comma separated list of examples to run"
          <> value Nothing
      )

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  config <- execParser (info (configParser curDir) fullDesc)
  setCurrentDirectory $ cfgDir config
  files <- listDirectory "."
  let filterOnly =
        case cfgOnly config of
          Nothing ->
            const True
          Just names ->
            \file -> takeBaseName file `elem` names
  results <- for (filter (\file -> ".dhall" == takeExtension file && filterOnly file) files) $ \file -> do
    example <- Dhall.inputFile (exampleDecoder file) file
    runExample config example
  let (failures, successes) = Either.partitionEithers results
  putStrLn $ show (length successes) <> " examples passed"
  putStrLn $ show (length failures) <> " examples failed"
  traverse_ putStrLn failures
  case failures of
    [] -> exitSuccess
    _ : _ -> exitFailure
