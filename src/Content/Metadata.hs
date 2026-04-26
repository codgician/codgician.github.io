module Content.Metadata
  ( metadataBool,
    metadataString,
    metadataStringOrEmpty,
    metadataStringList,
    featuresFromMetadata,
    templateFromMetadata,
    slideLevelFromMetadata,
  )
where

import Content.Types (RenderFeatures (..))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Hakyll (Metadata, lookupString, lookupStringList)
import Text.Read (readMaybe)

-- | Check if a metadata field is set to "true" (case-insensitive)
metadataBool :: String -> Metadata -> Bool
metadataBool key meta = maybe False ((== "true") . map toLower) $ lookupString key meta

-- | Look up an optional string metadata field
metadataString :: String -> Metadata -> Maybe String
metadataString = lookupString

-- | Look up a string metadata field with empty string fallback
metadataStringOrEmpty :: String -> Metadata -> String
metadataStringOrEmpty key = fromMaybe "" . metadataString key

-- | Look up a list of strings metadata field with empty list fallback
metadataStringList :: String -> Metadata -> [String]
metadataStringList key meta = fromMaybe [] $ lookupStringList key meta

-- | Build RenderFeatures from document metadata
featuresFromMetadata :: Metadata -> RenderFeatures
featuresFromMetadata meta =
  RenderFeatures
    { renderMath = metadataBool "math" meta,
      renderMermaid = metadataBool "mermaid" meta,
      renderToc = metadataBool "toc" meta
    }

-- | Get the template name from metadata, defaulting to "page"
templateFromMetadata :: Metadata -> String
templateFromMetadata meta = fromMaybe "page" $ metadataString "template" meta

-- | Get the slide level from metadata, defaulting to 2
slideLevelFromMetadata :: Metadata -> Int
slideLevelFromMetadata meta = fromMaybe 2 $ metadataString "slide-level" meta >>= readMaybe
