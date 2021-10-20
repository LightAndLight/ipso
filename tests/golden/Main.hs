{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Ipso.Common (Config(..), displayError, examplesMain, eqExitCode)
import Control.Monad (unless)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Foldable (for_)
import Data.Semigroup (Option (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Vector (Vector)
import qualified Dhall
import Options.Applicative (Parser, help, long, metavar, strOption)
import qualified System.IO
import qualified System.IO.Temp as Temp
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

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

runDiff :: Text -> Text -> IO (Maybe String)
runDiff expected actual = 
  Temp.withSystemTempFile "expected.txt" $ \expectedTmpPath expectedTmpHandle -> 
    Temp.withSystemTempFile "actual.txt" $ \actualTmpPath actualTmpHandle -> do
      Text.IO.hPutStrLn expectedTmpHandle expected <* System.IO.hClose expectedTmpHandle
      Text.IO.hPutStrLn actualTmpHandle actual <* System.IO.hClose actualTmpHandle
      (ec, out, _) <- readProcessWithExitCode "diff" ["-u", "--color=always", "--label", "expected", "--label", "actual", actualTmpPath, expectedTmpPath] ""
      if ec == ExitSuccess 
        then pure Nothing 
        else pure $ Just out

runExample :: Config String -> Example -> IO (Either String ())
runExample config example = do
  let args = foldr ((:) . Text.unpack) [] $ exArgs example
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      (cfgRest config)
      args
      (maybe "" Text.unpack $ exStdin example)

  errorRef <- newIORef $ Option Nothing

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

  Option mError <- readIORef errorRef
  pure $
    case mError of
      Just err -> Left err
      Nothing -> Right ()

binParser :: Parser String
binParser =
    strOption
      ( long "bin"
          <> help "path to binary"
          <> metavar "PATH"
      )

main :: IO ()
main = examplesMain binParser exampleDecoder runExample