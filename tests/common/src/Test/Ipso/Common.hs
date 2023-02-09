{-# LANGUAGE OverloadedStrings #-}

module Test.Ipso.Common (runDiff, eqExitCode, displayError, Config (..), configParser, examplesMain) where

import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import qualified Dhall
import Options.Applicative (Parser, ReadM, execParser, fullDesc, help, info, long, metavar, option, str, strOption, value)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeExtension, (</>))
import qualified System.IO
import qualified System.IO.Temp as Temp
import System.Process (readProcessWithExitCode)

runDiff :: Text -> Text -> IO (Maybe String)
runDiff expected actual =
  Temp.withSystemTempFile "expected.txt" $ \expectedTmpPath expectedTmpHandle ->
    Temp.withSystemTempFile "actual.txt" $ \actualTmpPath actualTmpHandle -> do
      Text.IO.hPutStrLn expectedTmpHandle expected <* System.IO.hClose expectedTmpHandle
      Text.IO.hPutStrLn actualTmpHandle actual <* System.IO.hClose actualTmpHandle
      (ec, out, _) <- readProcessWithExitCode "diff" ["-u", "--color=always", "--label", "actual", "--label", "expected", actualTmpPath, expectedTmpPath] ""
      if ec == ExitSuccess
        then pure Nothing
        else pure $ Just out

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
    [ path <> ": " <> label <> " mismatch",
      "expected:",
      Text.unpack $ showExpected expected,
      "actual:",
      showActual actual
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
  { cfgDir :: FilePath,
    cfgOnly :: Maybe [String],
    cfgRest :: a
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

data SkipListEntry = SkipListEntry { skipListEntryName :: String, skipListEntryReason :: String }
  deriving Show

skipListDecoder :: Dhall.Decoder [SkipListEntry]
skipListDecoder =
  Dhall.list . Dhall.record $
    SkipListEntry
      <$> Dhall.field "name" Dhall.string
      <*> Dhall.field "reason" Dhall.string

shouldSkip :: [SkipListEntry] -> String -> Maybe String
shouldSkip skipListEntries name =
  skipListEntryReason <$> List.find (\entry -> skipListEntryName entry == name) skipListEntries

data Result a = Success a | Failure String | Skip String

partitionResults :: [Result a] -> ([a], [String], [String])
partitionResults = 
  foldr 
    (\item (successes, failures, skips) ->
      case item of
        Success success -> (success : successes, failures, skips)
        Failure failure -> (successes, failure : failures, skips)
        Skip skip -> (successes, failures, skip : skips)
    )
    ([], [], [])

examplesMain ::
  Parser a ->
  (FilePath -> Dhall.Decoder example) ->
  (Config a -> example -> IO (Either String x)) ->
  IO ()
examplesMain restParser exampleDecoder runExample = do
  curDir <- getCurrentDirectory
  config <- execParser (info (configParser curDir restParser) fullDesc)
  setCurrentDirectory $ cfgDir config

  let skipListFile = "__skip.dhall"
  skipListFileExists <- doesFileExist skipListFile
  skipList <-
    if skipListFileExists
      -- Assumes `skipList` has very few lines. For better performance use a set with O(log n) membership.
      then Dhall.inputFile skipListDecoder skipListFile
      else pure []
  
  files <- listDirectory "."
  
  let filterOnly =
        case cfgOnly config of
          Nothing ->
            const True
          Just names ->
            \file -> takeBaseName file `elem` names
  
  results <- for (List.sort $ filter (\file -> ".dhall" == takeExtension file && filterOnly file && file /= skipListFile) files) $ \file -> do
    let testName = takeBaseName file
    case shouldSkip skipList testName of
      Nothing -> do
        example <- Dhall.inputFile (exampleDecoder file) file
        either Failure Success <$> runExample config example
      Just reason -> do
        putStrLn $ "skipping " <> testName <> ": " <> reason
        pure $ Skip reason
  
  let (successes, failures, skips) = partitionResults results

  if null skips then pure () else putStrLn ""
  putStrLn $ show (length successes) <> " examples passed"
  putStrLn $ show (length failures) <> " examples failed"
  putStrLn $ show (length skips) <> " examples skipped"
  putStrLn ""
  traverse_ putStr failures
  case failures of
    [] -> exitSuccess
    _ : _ -> exitFailure
