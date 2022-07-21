{-# LANGUAGE OverloadedStrings #-}

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Hakyll
import System.FilePath (splitExtension)

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

main :: IO ()
main = hakyll $ do
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
      pandocCompiler
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
        >>= htmlizeUrls
  
  match "pages/docs/index.md" $ do
    route $ customRoute (const "docs.html")
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
        >>= htmlizeUrls

  match ("pages/docs/*" .&&. complement "pages/docs/index.md") $ do
    route $ setExtension "html" `composeRoutes` customRoute (fromJust . stripPrefix "pages/docs/" . toFilePath)
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
        >>= htmlizeUrls
