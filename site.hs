{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Hakyll hiding (fromList)
import System.FilePath
import Text.Pandoc.Builder
import Text.Pandoc.Options
import Text.Pandoc.Walk

postCompiler :: Compiler (Item String)
postCompiler = fmap (withUrls rewriteOrgUrl . demoteHeaders) <$> pandocCompiler
  where
    rewriteOrgUrl url = maybe url (`addExtension` ".html") (stripExtension ".org" url)

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "js/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ postCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let postsCtx =
              constField "title" "Posts" <>
              listField "posts" postCtx (return posts) <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" postsCtx
          >>= loadAndApplyTemplate "templates/default.html" postsCtx
          >>= relativizeUrls

    match "projects.org" $ do
      route $ setExtension "html"
      let saveProjectTitles p = do
            projects <- writePandoc <$> makeItem (extractHeaders 10 p)
            void $ saveSnapshot "headers" projects
            pure p
      compile $ pandocCompilerWithTransformM def def saveProjectTitles
        >>= loadAndApplyTemplate "templates/default.html" postCtx

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        let feedCtx = postCtx <> bodyField "description"
        renderRss feedConfig feedCtx posts

    match "index.html" $ do
      route idRoute
      compile $ do
        latestPosts <- take 10 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
        projects <- loadSnapshotBody "projects.org" "headers"
        let indexCtx =
              constField "title" "Home" <>
              constField "projects" projects <>
              listField "posts" postCtx (return latestPosts) <>
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" (compile templateBodyCompiler)

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "public"
  , deployCommand = "rsync -Pavz --delete ./public deque:/data/myme.no"
  }

postCtx :: Context String
postCtx = dateField "date" "%F" <> defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "myme.no tech blog"
  , feedDescription = "Blog posts with technincal and programming related content."
  , feedAuthorName = "Martin Myrseth"
  , feedAuthorEmail = "myrseth@gmail.com"
  , feedRoot = "https://myme.no"
  }

-- | Extract headers from a Pandoc document
extractHeaders :: Int -> Pandoc -> Pandoc
extractHeaders n p = doc (bulletList links)
  where
    links = plain . fromList . take n <$> query headers p
    headers (Header lvl _ content) | lvl > 1 = [content]
    headers _ = []
