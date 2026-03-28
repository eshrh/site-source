--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_, when)
import Data.Functor
import Data.Functor.Identity (runIdentity)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Set (insert)
import qualified Data.Text as T
import Hakyll
import Hakyll.Web.Html (demoteHeaders)
import Skylighting (styleToCss, monochrome)
import Skylighting.Styles
import Data.Time
  ( Day
  , UTCTime(..)
  , ZonedTime
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , hoursToTimeZone
  , parseTimeM
  , secondsToDiffTime
  , utcToZonedTime
  )
import System.Directory
  ( copyFile
  , getHomeDirectory
  , doesFileExist
  , createDirectoryIfMissing
  , listDirectory
  )
import System.FilePath (FilePath, joinPath, takeBaseName, takeExtension)
import System.Posix.Internals (newFilePath)

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Options ( ReaderOptions (..)
                           , WriterOptions (..)
                           , HTMLMathMethod (..))
import Text.Pandoc.Templates (compileTemplate)
import Text.Pandoc.Extensions
import qualified Text.Pandoc.Templates (Template)

--------------------------------------------------------------------------------
pandocCodeStyle = monochrome

-- tocTemplate :: Text.Pandoc.Templates.Template Text
tocTemplate =
  either error id . runIdentity . compileTemplate "" $
    T.unlines
      [ "<div class=\"toc\">"
      , "$if(toc)$<h2 class=\"tocheader\">Contents</h2>$endif$"
      , "$toc$"
      , "</div>"
      , "$body$"
      ]

extraExts :: Extensions
extraExts = extensionsFromList
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_inline_code_attributes
            ]

pandocCompilerWithOpts :: Compiler (Item String)
pandocCompilerWithOpts =
  pandocCompilerWith
    defaultHakyllReaderOptions
      { readerExtensions = readerExtensions defaultHakyllReaderOptions
                           <> extraExts
      }
    defaultHakyllWriterOptions
      { writerTableOfContents = True
      , writerNumberSections = True
      , writerTOCDepth = 2
      , writerTemplate = Just tocTemplate
      , writerHTMLMathMethod = MathJax ""
      , writerExtensions = getDefaultExtensions "ipynb"
      , writerHighlightStyle   = Just pandocCodeStyle
      }

expandHome :: FilePath -> String -> FilePath
expandHome home s
  | "~" `isPrefixOf` s = joinPath [home, drop 2 s]
  | otherwise = s

syncOne :: FilePath -> [String] -> IO ()
syncOne home item = do
  let path = expandHome home (head item)
  exists <- doesFileExist path
  when exists (copyFile path (last item))

formatIsoWithColon :: ZonedTime -> String
formatIsoWithColon zt =
  let raw = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" zt
   in if length raw > 2
        then take (length raw - 2) raw <> ":" <> drop (length raw - 2) raw
        else raw

parsePostDay :: FilePath -> Maybe Day
parsePostDay path =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 (takeBaseName path))

latestPostTime :: IO (Maybe UTCTime)
latestPostTime = do
  files <- listDirectory "posts"
  let candidates =
        filter
          (\path -> takeExtension path == ".org" || takeExtension path == ".md")
          files
  let days = mapMaybe parsePostDay candidates
  case days of
    [] -> return Nothing
    _ -> return $ Just $ UTCTime (maximum days) (secondsToDiffTime 0)

pageTitleField :: Context String
pageTitleField = field "pageTitle" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  case lookupString "title" metadata of
    Just title -> return title
    Nothing ->
      return $ takeBaseName $ toFilePath $ itemIdentifier item

statusCtx :: Context String
statusCtx =
  field "lastPostIso" (\_ -> unsafeCompiler $ do
    latest <- latestPostTime
    case latest of
      Just utc -> do
        let jst = utcToZonedTime (hoursToTimeZone 9) utc
        return $ formatIsoWithColon jst
      Nothing -> return ""
  )
    <> field "syncIso" (\_ -> unsafeCompiler $ do
      now <- getCurrentTime
      let jst = utcToZonedTime (hoursToTimeZone 9) now
      return $ formatIsoWithColon jst
    )

postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d"
    <> pageTitleField
    <> statusCtx
    <> constField "relay" "SLAVE"
    <> defaultContext

siteCtx :: Context String
siteCtx = pageTitleField <> statusCtx <> constField "relay" "MASTER" <> defaultContext

main :: IO ()
main = do
  homedir <- getHomeDirectory
  syncFilesList <- readFile "./syncFiles.txt"
  let syncFiles = map words (lines syncFilesList)
  mapM_ (syncOne homedir) syncFiles

  createDirectoryIfMissing True "_site/css"
  writeFile "_site/css/syntax.css" $ styleToCss pandocCodeStyle

  hakyll $ do
    match "images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "favicon.svg" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile copyFileCompiler

    match "js/*" $ do
      route idRoute
      compile copyFileCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithOpts
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    match "pages/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithOpts
          >>= loadAndApplyTemplate "templates/default.html" siteCtx
          >>= relativizeUrls

    create ["archive.html"] $ do
      route $ constRoute "pages/archive.html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                <> constField "title" "archives"
                <> constField "pageTitle" "archives"
                <> constField "relay" "ARCHIVE"
                <> siteCtx

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                <> constField "isIndex" "true"
                <> constField "relay" "MASTER"
                <> siteCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss pandocCodeStyle

    match "_headers" $ do
      route idRoute
      compile copyFileCompiler

    create ["feed"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 3) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderRss feedConfig feedCtx posts
          where
            feedConfig =
              FeedConfiguration
              { feedTitle       = "esrh.me"
              , feedAuthorEmail = "esrh@esrh.me"
              , feedRoot        = "https://esrh.me"
              , feedDescription = "esrh blog feed"
              , feedAuthorName = "Eshan Ramesh"
              }
