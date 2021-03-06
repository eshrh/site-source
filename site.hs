--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_)
import Data.Functor
import Data.Functor.Identity (runIdentity)
import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
import Hakyll.Web.Html (demoteHeaders)
import Skylighting (styleToCss, zenburn)
import Skylighting.Styles
import System.Directory
  ( copyFile,
    getHomeDirectory,
  )
import System.FilePath (FilePath, joinPath)
import System.Posix.Internals (newFilePath)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Templates (compileTemplate)
import qualified Text.Pandoc.Templates (Template)

--------------------------------------------------------------------------------
pandocCodeStyle = zenburn

tocTemplate :: Text.Pandoc.Templates.Template Text
tocTemplate =
  either error id . runIdentity . compileTemplate "" $
    T.unlines
      [ "<h2 class=\"tocheader\">Contents</h2>",
        "<div class=\"toc\">",
        "$toc$",
        "</div>",
        "$body$"
      ]

pandocCompilerWithOpts :: Compiler (Item String)
pandocCompilerWithOpts =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerTableOfContents = True,
        writerNumberSections = True,
        writerTOCDepth = 2,
        writerTemplate = Just tocTemplate
      }

expandHome :: FilePath -> String -> FilePath
expandHome home s
  | "~" `isPrefixOf` s = joinPath [home, drop 2 s]
  | otherwise = s

syncOne :: FilePath -> [String] -> IO ()
syncOne home item = copyFile (expandHome home (head item)) (last item)

main :: IO ()
main = do
  homedir <- getHomeDirectory
  syncFilesList <- readFile "./syncFiles.txt"
  let syncFiles = map words (lines syncFilesList)
  mapM_ (syncOne homedir) syncFiles

  writeFile "_site/css/syntax.css" $ styleToCss zenburn

  hakyll $ do
    match "images/*" $ do
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
          >>= loadAndApplyTemplate "templates/post.html" postCtx . fmap demoteHeaders
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

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%Y / %m / %d"
    <> defaultContext
