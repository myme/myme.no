{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Char as Char
import Data.Function ((&))
import Data.List (isInfixOf, isPrefixOf)
import Hakyll hiding (fromList)
import System.FilePath
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.HTML.TagSoup (Tag (..))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Read (readMaybe)

frontpagePosts :: Int
frontpagePosts = 20

toLower :: Functor f => f Char.Char -> f Char.Char
toLower = fmap Char.toLower

-- | Compiler for each blog post page
postCompiler :: (Pandoc -> Compiler Pandoc) -> Compiler (Item String)
postCompiler transform = do
  ident <- getUnderlying
  toc <- getMetadataField ident "toc"
  let writerOpts = case toc >>= readMaybe of
        Nothing -> defaultHakyllWriterOptions
        Just depth ->
          defaultHakyllWriterOptions
            { writerTableOfContents = True,
              writerTOCDepth = depth,
              writerTemplate = Just tocTemplate
            }
      pandoc = pandocCompilerWithTransformM defaultHakyllReaderOptions
  fmap (withTagList convertVideoLinks . withUrls rewriteOrgUrl . demoteHeaders) <$> pandoc writerOpts transform
  where
    tocTemplate =
      either error id $
        either (error . show) id $
          Pandoc.runPure $
            Pandoc.runWithDefaultPartials $
              Pandoc.compileTemplate "" "<div class=\"toc\"><h1>Contents</h1>\n$toc$\n</div>\n$body$"

-- | Rewrite URLs to (local) .org files to .html.
rewriteOrgUrl :: String -> String
rewriteOrgUrl url
  | not ("://" `isInfixOf` url) =
    split "::" url & \case
      [] -> url
      [u] -> orgToHtml u
      (u : section : _) -> orgToHtml u <> toAnchor section
  | otherwise = url
  where
    orgToHtml u = maybe u (`addExtension` ".html") (stripExtension ".org" u)

split :: String -> String -> [String]
split delim = split' [] []
  where
    split' accum strings [] = reverse (reverse accum : strings)
    split' accum strings i@(c : rest)
      | delim `isPrefixOf` i = split' [] (reverse accum : strings) (drop (length delim) i)
      | otherwise = split' (c : accum) strings rest

toAnchor :: String -> String
toAnchor = \case
  ('*' : rest) -> '#' : map (\case ' ' -> '-'; x -> Char.toLower x) rest
  input -> input

-- | Returns true for any post which is not a preview
--
-- Useful for hiding experimental posts from the frontpage, posts & feed.xml.
-- Posts are still built and accessible, but only through direct link.
postIsNotPreview :: Item String -> Compiler Bool
postIsNotPreview item = do
  preview <- getMetadataField (itemIdentifier item) "preview"
  pure (preview /= Just "true")

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
    route idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  tags <- buildTags "posts/*" (fromCapture "tags/*.html" . toLower)

  -- Individual posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      let renderLink _ Nothing = Nothing
          renderLink tag (Just url) = Just $
            H.li $
              H.a
                ! ( A.href (H.toValue ("/" <> url))
                      <> A.title ("Navigate posts by tag: " <> H.stringValue tag))
                $ do
                  H.span ! A.class_ "fa fa-tag" $ " "
                  " "
                  H.toHtml tag
          tagsCtx = tagsFieldWith getTags renderLink mconcat "tags" tags
      postCompiler pure
        >>= loadAndApplyTemplate "templates/post.html" (tagsCtx <> postCtx)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- Full posts listing
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <-
        loadAll "posts/*"
          >>= filterM postIsNotPreview
          >>= recentFirst
      let postsCtx =
            constField "title" "Posts"
              <> listField "posts" postCtx (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  -- Individual tag pages
  tagsRules tags $ \tagStr tagsPattern -> do
    route idRoute
    compile $ do
      posts <- loadAll tagsPattern >>= filterM postIsNotPreview >>= recentFirst
      let postsCtx =
            constField "title" tagStr
              <> listField "posts" postCtx (return posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  match "*.org" $ do
    route $ setExtension "html"
    let saveHeaders p = do
          projects <- writePandoc <$> makeItem (extractHeaders 10 p)
          void $ saveSnapshot "headers" projects
          pure p
    compile $
      postCompiler saveHeaders
        >>= loadAndApplyTemplate "templates/other.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  create ["atom-feed.xml", "rss-feed.xml"] $ do
    route idRoute
    compile $ do
      publicPosts <-
        loadAllSnapshots "posts/*" "content"
          >>= filterM postIsNotPreview
          >>= recentFirst
      let feedCtx = postCtx <> bodyField "description"
      isAtom <- ("atom" `isPrefixOf`) . toFilePath <$> getUnderlying
      if isAtom
        then renderAtom feedConfig feedCtx publicPosts
        else do
          -- Override default timestamp formats from Hakyll
          let fmt = "%Y-%m-%dT%H:%M:%SZ"
          renderRss feedConfig (feedCtx <> dateField "published" fmt <> dateField "updated" fmt) publicPosts

  match "index.html" $ do
    route idRoute
    compile $ do
      publicPosts <-
        loadAllSnapshots "posts/*" "content"
          >>= filterM postIsNotPreview
          >>= recentFirst
      projects <- loadSnapshotBody "projects.org" "headers"
      let indexCtx =
            constField "title" "Home"
              <> constField "projects" projects
              <> listField "posts" postCtx (pure $ take frontpagePosts publicPosts)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" (compile templateBodyCompiler)

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "public"
    }

postCtx :: Context String
postCtx = dateField "date" "%F" <> defaultContext

feedConfig :: FeedConfiguration
feedConfig =
  FeedConfiguration
    { feedTitle = "myme.no tech blog",
      feedDescription = "Blog posts with technincal and programming related content.",
      feedAuthorName = "Martin Myrseth",
      feedAuthorEmail = "myrseth@gmail.com",
      feedRoot = "https://myme.no"
    }

-- | Extract headers from a Pandoc document
extractHeaders :: Int -> Pandoc -> Pandoc
extractHeaders n p = doc (bulletList links)
  where
    links = plain . fromList . take n <$> query headers p
    headers (Header lvl _ content) | lvl > 1 = [content]
    headers _ = []
