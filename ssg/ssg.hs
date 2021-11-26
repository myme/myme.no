{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Hakyll hiding (fromList)
import           System.FilePath
import           Text.HTML.TagSoup (Tag(..))
import qualified Text.Pandoc as Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Options
import           Text.Pandoc.Walk
import           Text.Read (readMaybe)

frontpagePosts :: Int
frontpagePosts = 20

-- | Compiler for each blog post page
postCompiler :: Compiler (Item String)
postCompiler = do
  ident <- getUnderlying
  toc <- getMetadataField ident "toc"
  let writerOpts = case toc >>= readMaybe of
        Nothing -> defaultHakyllWriterOptions
        Just depth -> defaultHakyllWriterOptions
          { writerTableOfContents = True
          , writerTOCDepth = depth
          , writerTemplate = Just tocTemplate
          }
      pandoc = pandocCompilerWith defaultHakyllReaderOptions
  fmap (withTagList convertVideoLinks . withUrls rewriteOrgUrl . demoteHeaders) <$> pandoc writerOpts
  where
    rewriteOrgUrl url = maybe url (`addExtension` ".html") (stripExtension ".org" url)
    tocTemplate = either error id $ either (error . show) id $
      Pandoc.runPure $ Pandoc.runWithDefaultPartials $
      Pandoc.compileTemplate "" "<div class=\"toc\"><h1>Contents</h1>\n$toc$\n</div>\n$body$"

-- | Convert links to videos to <video> HTML elements
--
-- Pandoc rewrites all org links to <a> tags. For video files this should rather
-- be an embedded video HTML element.
convertVideoLinks :: [Tag String] -> [Tag String]
convertVideoLinks (TagOpen "a" attrs : TagText txt : TagClose "a" : rest) =
  case videoUrl of
    Just url ->
      TagOpen "video" (("src", url) : defVideoAttrs) :
      TagClose "video" :
      convertVideoLinks rest
    _ -> TagOpen "a" attrs : TagText txt : TagClose "a" : convertVideoLinks rest
  where
    defVideoAttrs = [("autoplay", ""), ("controls", ""), ("loop", "")]
    videoUrl = case splitExtension <$> lookup "href" attrs of
      Just (path, ".webm") -> Just $ path <> ".webm"
      _ -> Nothing
convertVideoLinks (tag : rest) = tag : convertVideoLinks rest
convertVideoLinks [] = []

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
        latestPosts <- take frontpagePosts <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
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
