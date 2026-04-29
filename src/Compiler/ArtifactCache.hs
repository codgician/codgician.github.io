{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.ArtifactCache
  ( ArtifactCache (..),
    ArtifactPath (..),
    ArtifactRenderer (..),
    ArtifactRoot (..),
    CacheEvent (..),
    CacheInput (..),
    CacheKey (..),
    CacheLookup (..),
    CacheMissReason (..),
    CacheSchema (..),
    RecipeOption (..),
    RecipeVersion (..),
    RendererId,
    RendererIdError (..),
    RendererRecipe (..),
    ToolVersion (..),
    artifactPath,
    cacheKey,
    mkRendererId,
    renderCached,
    rendererIdText,
  )
where

import Control.Exception (IOException, catch, throwIO, try)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removeFile,
    renameFile,
  )
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hFlush)
import System.IO.Temp (withTempFile)

newtype RendererId = RendererId {rendererIdText :: Text}
  deriving (Eq, Ord, Show)

data RendererIdError
  = EmptyRendererId
  | UnsafeRendererId Text
  deriving (Eq, Show)

newtype ArtifactRoot = ArtifactRoot FilePath
  deriving (Eq, Show)

newtype ArtifactPath = ArtifactPath FilePath
  deriving (Eq, Show)

newtype CacheKey = CacheKey Text
  deriving (Eq, Ord, Show)

newtype CacheInput = CacheInput Text
  deriving (Eq, Show)

newtype CacheSchema = CacheSchema Text
  deriving (Eq, Show)

newtype RecipeVersion = RecipeVersion Text
  deriving (Eq, Show)

newtype ToolVersion = ToolVersion Text
  deriving (Eq, Show)

newtype RecipeOption = RecipeOption Text
  deriving (Eq, Show)

data ArtifactCache = ArtifactCache
  { artifactRoot :: ArtifactRoot,
    artifactSchema :: CacheSchema,
    artifactReporter :: CacheEvent -> IO ()
  }

data RendererRecipe = RendererRecipe
  { recipeId :: RendererId,
    recipeToolVersion :: ToolVersion,
    recipeVersion :: RecipeVersion,
    recipeOptions :: [RecipeOption]
  }
  deriving (Eq, Show)

data ArtifactRenderer input = ArtifactRenderer
  { artifactRecipe :: RendererRecipe,
    artifactCacheInput :: input -> CacheInput,
    artifactRender :: input -> IO Text
  }

data CacheLookup
  = CacheHit Text
  | CacheMiss CacheMissReason
  deriving (Eq, Show)

data CacheMissReason
  = ArtifactMissing
  | ArtifactUnreadable FilePath String
  | ArtifactInvalidUtf8 FilePath String
  deriving (Eq, Show)

data CacheEvent
  = ArtifactRecovered RendererId CacheKey CacheMissReason
  | ArtifactWriteRace RendererId CacheKey
  deriving (Eq, Show)

mkRendererId :: Text -> Either RendererIdError RendererId
mkRendererId value
  | T.null value = Left EmptyRendererId
  | T.any unsafeChar value = Left $ UnsafeRendererId value
  | "." `elem` segments = Left $ UnsafeRendererId value
  | ".." `elem` segments = Left $ UnsafeRendererId value
  | otherwise = Right $ RendererId value
  where
    unsafeChar c = c == '/' || c == '\\' || c == ':'
    segments = T.splitOn "/" value

cacheKey :: ArtifactCache -> ArtifactRenderer input -> input -> CacheKey
cacheKey cache renderer input =
  CacheKey $ TE.decodeUtf8 $ B16.encode digest
  where
    digest = hash $ TE.encodeUtf8 $ T.concat $ map encodePart $ keyParts cache renderer input

keyParts :: ArtifactCache -> ArtifactRenderer input -> input -> [Text]
keyParts cache renderer input =
  [ schema,
    rendererIdText rendererId,
    toolVersion,
    version
  ]
    <> options
    <> [cacheInput]
  where
    CacheSchema schema = artifactSchema cache
    recipe = artifactRecipe renderer
    rendererId = recipeId recipe
    ToolVersion toolVersion = recipeToolVersion recipe
    RecipeVersion version = recipeVersion recipe
    options = map (\(RecipeOption option) -> option) $ recipeOptions recipe
    CacheInput cacheInput = artifactCacheInput renderer input

encodePart :: Text -> Text
encodePart value = T.pack (show $ T.length value) <> ":" <> value <> ";"

artifactPath :: ArtifactCache -> ArtifactRenderer input -> CacheKey -> ArtifactPath
artifactPath cache renderer (CacheKey keyText) =
  ArtifactPath $
    root
      </> T.unpack (rendererIdText $ recipeId $ artifactRecipe renderer)
      </> T.unpack (T.take 2 keyText)
      </> T.unpack (T.take 2 $ T.drop 2 keyText)
      </> T.unpack keyText <.> "html"
  where
    ArtifactRoot root = artifactRoot cache

renderCached :: ArtifactCache -> ArtifactRenderer input -> input -> IO Text
renderCached cache renderer input = do
  let key = cacheKey cache renderer input
      path = artifactPath cache renderer key
  lookupResult <- readArtifact path
  case lookupResult of
    CacheHit html -> pure html
    CacheMiss reason -> do
      rendered <- artifactRender renderer input
      writeArtifactAtomically cache renderer key path rendered
      reportRecovery cache renderer key reason
      pure rendered

readArtifact :: ArtifactPath -> IO CacheLookup
readArtifact (ArtifactPath path) = do
  exists <- doesFileExist path
  if exists
    then do
      bytesResult <- try $ BS.readFile path
      case bytesResult of
        Left (ex :: IOException) -> pure $ CacheMiss $ ArtifactUnreadable path $ show ex
        Right bytes ->
          case TE.decodeUtf8' bytes of
            Left err -> pure $ CacheMiss $ ArtifactInvalidUtf8 path $ show err
            Right text -> pure $ CacheHit text
    else pure $ CacheMiss ArtifactMissing

reportRecovery :: ArtifactCache -> ArtifactRenderer input -> CacheKey -> CacheMissReason -> IO ()
reportRecovery _ _ _ ArtifactMissing = pure ()
reportRecovery cache renderer key reason =
  artifactReporter cache $ ArtifactRecovered (recipeId $ artifactRecipe renderer) key reason

writeArtifactAtomically :: ArtifactCache -> ArtifactRenderer input -> CacheKey -> ArtifactPath -> Text -> IO ()
writeArtifactAtomically cache renderer key path@(ArtifactPath finalPath) rendered = do
  createDirectoryIfMissing True $ takeDirectory finalPath
  withTempFile (takeDirectory finalPath) "artifact.tmp" $ \tmpPath tmpHandle -> do
    BS.hPut tmpHandle $ TE.encodeUtf8 rendered
    hFlush tmpHandle
    renameFile tmpPath finalPath `catch` handleRenameFailure tmpPath
  where
    handleRenameFailure :: FilePath -> IOException -> IO ()
    handleRenameFailure tmpPath original = do
      existing <- readArtifact path
      case existing of
        CacheHit _ -> do
          artifactReporter cache $ ArtifactWriteRace (recipeId $ artifactRecipe renderer) key
          removeFile tmpPath `catch` \(_ :: IOException) -> pure ()
        CacheMiss _ -> throwIO original
