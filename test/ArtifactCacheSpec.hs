{-# LANGUAGE OverloadedStrings #-}

module ArtifactCacheSpec (spec) where

import Compiler.ArtifactCache
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Compiler.ArtifactCache" $ do
  describe "mkRendererId" $ do
    it "accepts safe path segment renderer IDs" $ do
      rendererIdText <$> mkRendererId "graphviz-dot" `shouldBe` Right "graphviz-dot"
      rendererIdText <$> mkRendererId "d2" `shouldBe` Right "d2"

    it "rejects empty, absolute, parent, and slash-containing IDs" $ do
      mkRendererId "" `shouldSatisfy` isLeft
      mkRendererId "/katex" `shouldSatisfy` isLeft
      mkRendererId "../katex" `shouldSatisfy` isLeft
      mkRendererId "katex/html" `shouldSatisfy` isLeft

  describe "cacheKey" $ do
    it "is stable for identical recipe and input" $ do
      let key1 = cacheKey testCache testRenderer "x + y"
          key2 = cacheKey testCache testRenderer "x + y"
      key1 `shouldBe` key2

    it "changes when cache input changes" $
      cacheKey testCache testRenderer "x + y"
        `shouldNotBe` cacheKey testCache testRenderer "x - y"

    it "changes when schema changes" $ do
      let newerCache = testCache {artifactSchema = CacheSchema "v2"}
      cacheKey testCache testRenderer "x"
        `shouldNotBe` cacheKey newerCache testRenderer "x"

    it "changes when recipe options change" $ do
      let changedRenderer =
            testRenderer
              { artifactRecipe =
                  (artifactRecipe testRenderer)
                    { recipeOptions = [RecipeOption "--display-mode"]
                    }
              }
      cacheKey testCache testRenderer "x"
        `shouldNotBe` cacheKey testCache changedRenderer "x"

    it "uses a full SHA256 hex digest" $ do
      let CacheKey keyText = cacheKey testCache testRenderer "x"
      T.length keyText `shouldBe` 64
      T.all (`elem` ("0123456789abcdef" :: String)) keyText `shouldBe` True

  describe "artifactPath" $
    it "uses renderer ID, two shard levels, full key, and html extension" $ do
      let key@(CacheKey keyText) = cacheKey testCache testRenderer "x"
          ArtifactPath path = artifactPath testCache testRenderer key
          firstShard = T.unpack $ T.take 2 keyText
          secondShard = T.unpack $ T.take 2 $ T.drop 2 keyText
      path
        `shouldBe` "_artifacts/test-renderer/" <> firstShard <> "/" <> secondShard <> "/" <> T.unpack keyText <> ".html"

  describe "renderCached" $ do
    it "renders once and writes on miss" $
      withSystemTempDirectory "artifact-cache" $ \dir -> do
        counter <- newIORef (0 :: Int)
        let cache = testCache {artifactRoot = ArtifactRoot dir}
            renderer =
              testRenderer
                { artifactRender = \value -> modifyIORef' counter (+ 1) >> pure (T.pack value)
                }
        result <- renderCached cache renderer "fresh"
        count <- readIORef counter
        result `shouldBe` "fresh"
        count `shouldBe` 1

    it "returns cached output without calling renderer on hit" $
      withSystemTempDirectory "artifact-cache" $ \dir -> do
        let cache = testCache {artifactRoot = ArtifactRoot dir}
            renderer = testRenderer {artifactRender = const $ pure "first"}
        first <- renderCached cache renderer "same"
        second <- renderCached cache renderer {artifactRender = const $ expectationFailure "renderer should not run" >> pure "second"} "same"
        first `shouldBe` "first"
        second `shouldBe` "first"

    it "reports invalid UTF-8 and replaces the artifact" $
      withSystemTempDirectory "artifact-cache" $ \dir -> do
        eventsRef <- newIORef []
        let cache = testCache {artifactRoot = ArtifactRoot dir, artifactReporter = modifyIORef' eventsRef . (:)}
            key = cacheKey cache testRenderer "bad"
            ArtifactPath path = artifactPath cache testRenderer key
        createDirectoryIfMissing True $ takeDirectory path
        BS.writeFile path $ BS.pack [0x80]
        result <- renderCached cache testRenderer "bad"
        events <- readIORef eventsRef
        result `shouldBe` "bad"
        events `shouldSatisfy` any isRecovered

    it "reports recovery only after a replacement artifact is written" $
      withSystemTempDirectory "artifact-cache" $ \dir -> do
        eventsRef <- newIORef []
        let cache = testCache {artifactRoot = ArtifactRoot dir, artifactReporter = modifyIORef' eventsRef . (:)}
            renderer = testRenderer {artifactRender = const $ fail "renderer failed"}
            key = cacheKey cache renderer "bad"
            ArtifactPath path = artifactPath cache renderer key
        createDirectoryIfMissing True $ takeDirectory path
        BS.writeFile path $ BS.pack [0x80]
        renderCached cache renderer "bad" `shouldThrow` anyException
        readIORef eventsRef `shouldReturn` []

    it "propagates renderer failure and leaves no successful artifact" $
      withSystemTempDirectory "artifact-cache" $ \dir -> do
        let cache = testCache {artifactRoot = ArtifactRoot dir}
            renderer = testRenderer {artifactRender = const $ fail "renderer failed"}
            key = cacheKey cache renderer "boom"
            ArtifactPath path = artifactPath cache renderer key
        renderCached cache renderer "boom" `shouldThrow` anyException
        doesFileExist path `shouldReturn` False

isRecovered :: CacheEvent -> Bool
isRecovered ArtifactRecovered {} = True
isRecovered _ = False

testCache :: ArtifactCache
testCache =
  ArtifactCache
    { artifactRoot = ArtifactRoot "_artifacts",
      artifactSchema = CacheSchema "v1",
      artifactReporter = const $ pure ()
    }

testRenderer :: ArtifactRenderer String
testRenderer =
  ArtifactRenderer
    { artifactRecipe =
        RendererRecipe
          { recipeId = testRendererId,
            recipeToolVersion = ToolVersion "tool-1",
            recipeVersion = RecipeVersion "recipe-1",
            recipeOptions = [RecipeOption "--safe"]
          },
      artifactCacheInput = CacheInput . T.pack,
      artifactRender = pure . T.pack
    }

testRendererId :: RendererId
testRendererId =
  case mkRendererId "test-renderer" of
    Right rendererId -> rendererId
    Left err -> error $ "Invalid test renderer ID: " <> show err
