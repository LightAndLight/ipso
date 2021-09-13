{-# LANGUAGE OverloadedStrings #-}

module Test.Ipso.Common (eqExitCode, displayError, Config(..), configParser, examplesMain) where

import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Dhall
import Options.Applicative (Parser, ReadM, execParser, fullDesc, help, info, long, metavar, option, str, strOption, value)
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeExtension, (</>))

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


displayError :: String -> String -> (a, a -> Text) -> (b, b -> String) -> String
displayError label path (expected, showExpected) (actual, showActual) =
  unlines
    [ path <> ": " <> label <> " mismatch"
    , "expected:"
    , Text.unpack $ showExpected expected
    , "actual:"
    , showActual actual
    ]

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

data Config a = Config
  { cfgDir :: FilePath
  , cfgOnly :: Maybe [String]
  , cfgRest :: a
  }

configParser :: FilePath -> Parser a -> Parser (Config a)
configParser curDir restParser =
  (\dir only -> Config (curDir </> dir) only)
    <$> strOption
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
    <*> restParser

examplesMain :: 
  Parser a -> 
  (FilePath -> Dhall.Decoder example) -> 
  (Config a -> example -> IO (Either String x)) -> 
  IO ()
examplesMain restParser exampleDecoder runExample = do
  curDir <- getCurrentDirectory
  config <- execParser (info (configParser curDir restParser) fullDesc)
  setCurrentDirectory $ cfgDir config
  files <- listDirectory "."
  let filterOnly =
        case cfgOnly config of
          Nothing ->
            const True
          Just names ->
            \file -> takeBaseName file `elem` names
  results <- for (List.sort $ filter (\file -> ".dhall" == takeExtension file && filterOnly file) files) $ \file -> do
    example <- Dhall.inputFile (exampleDecoder file) file
    runExample config example
  let (failures, successes) = Either.partitionEithers results
  putStrLn $ show (length successes) <> " examples passed"
  putStrLn $ show (length failures) <> " examples failed"
  traverse_ putStrLn failures
  case failures of
    [] -> exitSuccess
    _ : _ -> exitFailure
