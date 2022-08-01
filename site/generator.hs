{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (for_)
import Data.List (stripPrefix)
import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified Data.Ord
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime)
import Hakyll
import Skylighting.Loader (loadSyntaxesFromDir)
import System.FilePath (normalise, replaceExtension, splitExtension, takeDirectory, (</>))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Pandoc.Options (writerSyntaxMap)

dateFormatString :: String
dateFormatString = "%-e %b, %Y"

htmlizeUrls :: Item String -> Compiler (Item String)
htmlizeUrls = pure . fmap (withUrls htmlize)
  where
    htmlize :: String -> String
    htmlize url =
      if isExternal url
        then url
        else
          let (file, fragment) = break (== '#') url
           in case splitExtension file of
                (name, ".md") -> (name ++ ".html") <> fragment
                _ -> url

nestUrls :: FilePath -> Item String -> Compiler (Item String)
nestUrls parent = pure . fmap (withUrls htmlize)
  where
    htmlize :: String -> String
    htmlize url =
      if isExternal url
        then url
        else case url of
          '#' : _ -> url
          _ -> normalise $ parent </> url

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
        let template =
              if "blog" `List.isPrefixOf` pathInPages
                then "templates/blog.html"
                else "templates/base.html"
        customPandocCompiler
          >>= loadAndApplyTemplate template (dateField "date" dateFormatString <> defaultContext)
          >>= nestUrls ("/" <> takeDirectory pathInPages)
          >>= htmlizeUrls

    create ["blog/index.html"] $ do
      route idRoute
      posts <- getMatches "pages/blog/*"
      postInfo <-
        traverse
          ( \post -> do
              title <- fromJust <$> getMetadataField post "title"
              date <- getItemUTC defaultTimeLocale post
              let url = flip replaceExtension "html" . fromJust . stripPrefix "pages/" $ toFilePath post
              pure (title, date, url)
          )
          posts
      let sortedPosts = List.sortOn (Data.Ord.Down . (\(_, date, _) -> date)) postInfo
      compile $ do
        makeItem (renderHtml $ postsHtml sortedPosts)
          >>= loadAndApplyTemplate
            "templates/base.html"
            (constField "title" "Blog" <> boolField "no-edit" (const True) <> defaultContext)
          >>= htmlizeUrls

postsHtml :: [(String, UTCTime, String)] -> Html
postsHtml posts = do
  Html.h1 "Posts"
  Html.table ! Attr.style "list-style: none; padding-left: 0;" $ do
    Html.tbody $ do
      for_ posts $ \(title, date, url) -> do
        Html.tr $ do
          Html.td $ do
            Html.span ! Attr.class_ "muted" $ do
              Html.string $ formatTime defaultTimeLocale dateFormatString date
          Html.td ! Attr.style "padding-left: 1em" $ do
            Html.a ! Attr.href (Html.toValue url) $ do
              Html.string title