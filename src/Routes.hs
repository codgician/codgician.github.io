module Routes
  ( RouteError,
    routeErrorMessage,
    routeOrFail,
    homeUrl,
    pageUrl,
    postUrl,
    slideUrl,
    feedUrl,
    sectionIndexUrl,
    sectionPageUrl,
    homeOutputPath,
    pageOutputPath,
    postOutputPath,
    slideOutputPath,
    feedOutputPath,
    sectionIndexOutputPath,
    sectionPageOutputPath,
    sectionPageIdentifier,
    contentAssetOutputPath,
    langFromIndexIdentifier,
    postSlugFromIdentifier,
    slideSlugFromIdentifier,
    slugFromSectionIdentifier,
    pageSegmentsFromIdentifier,
  )
where

import Content.Types
import Data.List (intercalate)
import qualified Data.Text as T
import Hakyll (Identifier, PageNumber, fromFilePath, toFilePath)
import System.FilePath (dropExtension, joinPath, splitDirectories, takeExtension, takeFileName, (</>))

newtype RouteError = RouteError String
  deriving (Eq, Show)

routeErrorMessage :: RouteError -> String
routeErrorMessage (RouteError msg) = msg

routeOrFail :: (MonadFail m) => Either RouteError a -> m a
routeOrFail = either (fail . routeErrorMessage) pure

homeUrl :: LangCode -> String
homeUrl lang = "/" <> langCodeString lang <> "/"

pageUrl :: LangCode -> [T.Text] -> String
pageUrl lang [] = homeUrl lang
pageUrl lang segments = "/" <> langCodeString lang <> "/" <> intercalate "/" (map T.unpack segments) <> "/"

postUrl :: LangCode -> Slug -> String
postUrl lang slug = "/" <> langCodeString lang <> "/posts/" <> slugString slug <> "/"

slideUrl :: LangCode -> Slug -> String
slideUrl lang slug = "/" <> langCodeString lang <> "/slides/" <> slugString slug <> "/"

feedUrl :: LangCode -> String
feedUrl lang = "/" <> langCodeString lang <> "/feed.xml"

sectionIndexUrl :: LangCode -> Section -> String
sectionIndexUrl lang section = "/" <> langCodeString lang <> "/" <> sectionPathString section <> "/"

sectionPageUrl :: LangCode -> Section -> PageNumber -> String
sectionPageUrl lang section pageNumber
  | pageNumber <= 1 = sectionIndexUrl lang section
  | otherwise = "/" <> langCodeString lang <> "/" <> sectionPathString section <> "/page/" <> show pageNumber <> "/"

homeOutputPath :: LangCode -> FilePath
homeOutputPath lang = langCodeString lang </> "index.html"

pageOutputPath :: LangCode -> [T.Text] -> FilePath
pageOutputPath lang [] = homeOutputPath lang
pageOutputPath lang segments = langCodeString lang </> joinPath (map T.unpack segments) </> "index.html"

postOutputPath :: LangCode -> Slug -> FilePath
postOutputPath lang slug = langCodeString lang </> "posts" </> slugString slug </> "index.html"

slideOutputPath :: LangCode -> Slug -> FilePath
slideOutputPath lang slug = langCodeString lang </> "slides" </> slugString slug </> "index.html"

feedOutputPath :: LangCode -> FilePath
feedOutputPath lang = langCodeString lang </> "feed.xml"

sectionIndexOutputPath :: LangCode -> Section -> FilePath
sectionIndexOutputPath lang section = langCodeString lang </> sectionPathString section </> "index.html"

sectionPageOutputPath :: LangCode -> Section -> PageNumber -> FilePath
sectionPageOutputPath lang section pageNumber
  | pageNumber <= 1 = sectionIndexOutputPath lang section
  | otherwise = langCodeString lang </> sectionPathString section </> "page" </> show pageNumber </> "index.html"

sectionPageIdentifier :: LangCode -> Section -> PageNumber -> Identifier
sectionPageIdentifier lang section = fromFilePath . sectionPageOutputPath lang section

contentAssetOutputPath :: LangCode -> Section -> Slug -> FilePath -> FilePath
contentAssetOutputPath lang section slug fileName =
  langCodeString lang </> sectionPathString section </> slugString slug </> fileName

langFromIndexIdentifier :: Identifier -> Either RouteError LangCode
langFromIndexIdentifier ident = parseIndexFileLang (takeFileName $ toFilePath ident)

postSlugFromIdentifier :: Identifier -> Either RouteError Slug
postSlugFromIdentifier = slugFromSectionIdentifier Posts

slideSlugFromIdentifier :: Identifier -> Either RouteError Slug
slideSlugFromIdentifier = slugFromSectionIdentifier Slides

slugFromSectionIdentifier :: Section -> Identifier -> Either RouteError Slug
slugFromSectionIdentifier section ident =
  case splitDirectories (toFilePath ident) of
    ["content", sectionDir, slug, _]
      | sectionDir == sectionPathString section && not (null slug) -> Right $ Slug $ T.pack slug
    parts ->
      Left $
        RouteError $
          "Expected content/" <> sectionPathString section <> "/<slug>/<file>, got: " <> joinPath parts

pageSegmentsFromIdentifier :: Identifier -> Either RouteError [T.Text]
pageSegmentsFromIdentifier ident =
  case splitDirectories (toFilePath ident) of
    "content" : rest -> pageSegmentsFromParts rest
    parts -> Left $ RouteError $ "Expected content/<path>/index.<lang>.md, got: " <> joinPath parts

pageSegmentsFromParts :: [FilePath] -> Either RouteError [T.Text]
pageSegmentsFromParts [] = Left $ RouteError "Expected localized page file under content/"
pageSegmentsFromParts [fileName] = [] <$ parseIndexFileLang fileName
pageSegmentsFromParts (dirName : rest) = (T.pack dirName :) <$> pageSegmentsFromParts rest

parseIndexFileLang :: FilePath -> Either RouteError LangCode
parseIndexFileLang fileName =
  let noMd = dropExtension fileName
      base = dropExtension noMd
      lang = drop 1 (takeExtension noMd)
   in case (base, lang, takeExtension fileName) of
        ("index", code@(_ : _), ".md") -> Right $ LangCode $ T.pack code
        _ -> Left $ RouteError $ "Expected index.<lang>.md, got: " <> fileName
