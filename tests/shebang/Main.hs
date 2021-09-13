{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Ipso.Common (Config(..), displayError, eqExitCode, examplesMain)
import Control.Monad (unless)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Semigroup (Option (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Dhall
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

data Example = Example
  { exPath :: FilePath
  , exDescription :: Text
  , exScript :: Text
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
        <*> Dhall.field "script" Dhall.strictText
        <*> Dhall.field "stdin" (Dhall.maybe Dhall.strictText)
        <*> Dhall.field "stdout" Dhall.strictText
        <*> Dhall.field "stderr" Dhall.strictText
        <*> Dhall.field "exitcode" Dhall.natural

runExample :: Config () -> Example -> IO (Either String ())
runExample config example = do
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode
      (cfgDir config </> Text.unpack (exScript example))
      []
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

main :: IO ()
main = examplesMain (pure ()) exampleDecoder runExample