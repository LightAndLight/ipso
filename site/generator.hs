{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Hakyll
import Skylighting.Loader (loadSyntaxesFromDir)
import System.FilePath (splitExtension)
import Text.Pandoc.Options (writerSyntaxMap)

htmlizeUrls :: Item String -> Compiler (Item String)
htmlizeUrls = pure . fmap (withUrls htmlize)
  where
    htmlize :: String -> String
    htmlize url =
      if isExternal url
        then url
        else case splitExtension url of
          (name, ".md") -> name ++ ".html"
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

    match "pages/*" $ do
      route $ setExtension "html" `composeRoutes` customRoute (fromJust . stripPrefix "pages/" . toFilePath)
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/base.html" defaultContext
          >>= relativizeUrls
          >>= htmlizeUrls

    match "pages/docs/index.md" $ do
      route $ customRoute (const "docs.html")
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/base.html" defaultContext
          >>= relativizeUrls
          >>= htmlizeUrls

    match ("pages/docs/*" .&&. complement "pages/docs/index.md") $ do
      route $ setExtension "html" `composeRoutes` customRoute (fromJust . stripPrefix "pages/docs/" . toFilePath)
      compile $
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/base.html" defaultContext
          >>= relativizeUrls
          >>= htmlizeUrls
