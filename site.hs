{-# LANGUAGE OverloadedStrings #-}

import Hakyll

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
      compile $ pandocCompiler
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

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        let feedCtx = postCtx <> bodyField "description"
        renderRss feedConfig feedCtx posts

    match "index.html" $ do
      route idRoute
      compile $ do
        latestPosts <- take 5 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
        let indexCtx =
              constField "title" "Home" <>
              listField "posts" postCtx (return latestPosts) <>
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

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
