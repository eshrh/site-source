--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_, when)
import Data.Functor
import Data.Functor.Identity (runIdentity)
import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Set (insert)
import qualified Data.Text as T
import Hakyll
import Hakyll.Web.Html (demoteHeaders)
import Skylighting (styleToCss, monochrome)
import Skylighting.Styles
import System.Directory
  ( copyFile
  , getHomeDirectory
  , doesFileExist
  , createDirectoryIfMissing
  )
import System.FilePath (FilePath, joinPath)
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

postCtx :: Context String
postCtx = dateField "date" "%Y / %m / %d" <> defaultContext

main :: IO ()
main = do
  homedir <- getHomeDirectory
  syncFilesList <- readFile "./syncFiles.txt"
  let syncFiles = map words (lines syncFilesList)
  mapM_ (syncOne homedir) syncFiles

  createDirectoryIfMissing True "_site/css"
  writeFile "_site/css/syntax.css" $ styleToCss pandocCodeStyle

  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "favicon.ico" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "files/*" $ do
      route idRoute
      compile copyFileCompiler

    match "pages/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithOpts
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    create ["archive.html"] $ do
      route $ constRoute "pages/archive.html"
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                <> constField "title" "archives"
                <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts)
                <> defaultContext

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
              , feedAuthorEmail = "esrh@gatech.edu"
              , feedRoot        = "https://esrh.me"
              , feedDescription = "my blog!"
              , feedAuthorName = "Eshan Ramesh"
              }
