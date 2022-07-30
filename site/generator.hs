{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Hakyll
import Skylighting.Loader (loadSyntaxesFromDir)
import System.FilePath ((</>), splitExtension, takeDirectory)
import Text.Pandoc.Options (writerSyntaxMap)

htmlizeUrls :: Item String -> Compiler (Item String)
htmlizeUrls = pure . fmap (withUrls htmlize)
  where
    htmlize :: String -> String
    htmlize url =
      if isExternal url
        then url
        else
          let (file, fragment) = break (== '#') url in
          case splitExtension file of
            (name, ".md") -> (name ++ ".html") <> fragment
            _ -> url

mkCustomPandocCompiler :: IO (Compiler (Item String))
mkCustomPandocCompiler = do
  syntaxes <- either error pure =<< loadSyntaxesFromDir "syntax"
  pure $
    pandocCompilerWith
      defaultHakyllReaderOptions
      defaultHakyllWriterOptions
        { writerSyntaxMap =
            writerSyntaxMap defaultHakyllWriterOptions
              <> syntaxes
        }

main :: IO ()
main = do
  customPandocCompiler <- mkCustomPandocCompiler
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "pages/**" $ do
      route $ setExtension "html" `composeRoutes` customRoute (fromJust . stripPrefix "pages/" . toFilePath)
      compile $ do
        pathInPages <- fromJust . stripPrefix "./pages/" <$> getResourceFilePath
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/base.html" defaultContext
          >>= htmlizeUrls