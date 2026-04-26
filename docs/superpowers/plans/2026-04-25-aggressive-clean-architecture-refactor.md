# Aggressive Clean Architecture Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor the Hakyll blog into a small, typed static-site compiler with centralized routing, explicit bilingual fallback semantics, focused rule modules, and a thin `Site.hs` composition root.

**Architecture:** Introduce domain types for language, slug, section, localized content, and render features; route every public URL/output path through `Routes`; move metadata/fallback/context/rule ownership out of `Site.hs`. Preserve current public URLs, template fields, snapshot names, Nix build behavior, and subprocess-based KaTeX/Mermaid/Sass integration.

**Tech Stack:** Haskell, Hakyll, HSpec, Nix flakes, Pandoc, KaTeX CLI, Mermaid CLI, Dart Sass.

---

## Scope and non-negotiable invariants

- Keep the current URL shapes:
  - `/en/`
  - `/zh/`
  - `/en/posts/<slug>/`
  - `/zh/posts/<slug>/`
  - `/en/slides/<slug>/`
  - `/zh/slides/<slug>/`
  - `/en/feed.xml`
  - `/zh/feed.xml`
  - `/en/<section>/page/<n>/`
- Every content item must be reachable at every configured language URL.
- If translated content does not exist, render the content's native/canonical source under the requested language URL.
- Do not return placeholder slugs or routes such as `"unknown"`; malformed identifiers must fail the build with clear errors.
- Preserve post snapshot names exactly: `"content"` and `"toc"`.
- Do not add runtime dependencies.
- Do not replace Hakyll with a custom rule DSL.
- Do not change KaTeX, Mermaid, or Sass from subprocess/Nix-managed tools.

---

## Target file structure

Create or reshape the source tree toward this structure:

```text
src/
  Site.hs
  Config.hs
  Context.hs
  Feed.hs
  Paginate.hs
  Routes.hs
  Content/
    Types.hs
    Metadata.hs
    Fallback.hs
  Rules/
    Static.hs
    Pages.hs
    Posts.hs
    Slides.hs
    Feeds.hs
    Sitemap.hs
  Compiler/
    Cache.hs
    KaTeX.hs
    Mermaid.hs
    Pandoc.hs
    Toc.hs

test/
  Spec.hs
  ConfigSpec.hs
  MetadataSpec.hs
  PaginateSpec.hs
  RoutesSpec.hs
```

`Site.hs` must end as a composition root: Hakyll configuration, config loading, dependency wrapping, and calls to rule groups only.

---

## Verification commands

Run these commands at the checkpoints named in the tasks:

```bash
nix build
```

```bash
nix flake check
```

If `nix flake check` is too slow during intermediate slices, run `nix build` after every slice and run `nix flake check` after Tasks 3, 6, 9, and 13.

Commit checkpoints in this plan require explicit user authorization in the implementation session. If that authorization is absent, skip the commit command and report the exact files changed.

---

### Task 1: Establish baseline and module inventory

**Files:**

- Read: `src/Site.hs`
- Read: `src/Config.hs`
- Read: `src/Context.hs`
- Read: `src/Feed.hs`
- Read: `src/Paginate.hs`
- Read: `package.yaml`
- Read: `builder.cabal`
- Read: `test/Spec.hs`

- [ ] **Step 1: Capture baseline build behavior**

Run:

```bash
nix build
```

Expected: the build succeeds. If it fails, stop and fix the baseline failure before starting the refactor.

- [ ] **Step 2: Capture baseline checks**

Run:

```bash
nix flake check
```

Expected: all configured checks pass. If checks fail but `nix build` passes, record the failing check name and fix it before changing architecture.

- [ ] **Step 3: Record the refactor boundaries**

Create a short implementation note in the session transcript with these boundaries:

```text
Site.hs will keep hakyllMain, config, and rules.
Routes.hs will own every public URL and output path.
Content.Types will own LangCode, Slug, Section, ContentRef, LocalizedRef, and RenderFeatures.
Content.Metadata will own metadata parsing.
Content.Fallback will own language fallback ordering.
Context.hs will own Hakyll template contexts.
Rules.* modules will own Hakyll rule groups.
Compiler.* modules will remain focused on rendering tools.
```

- [ ] **Step 4: Commit checkpoint if authorized**

Run only if commits were explicitly authorized:

```bash
git status --short
```

Expected: no refactor files have changed yet.

---

### Task 2: Add typed content primitives

**Files:**

- Create: `src/Content/Types.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Create `Content.Types`**

Create `src/Content/Types.hs` with this complete module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Content.Types
  ( LangCode (..),
    Slug (..),
    Section (..),
    ContentRef (..),
    LocalizedRef (..),
    RenderFeatures (..),
    langCodeString,
    slugString,
    sectionPathSegment,
    sectionPathString,
    sectionFromPathSegment,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

newtype LangCode = LangCode {unLangCode :: Text}
  deriving (Eq, Ord, Show)

newtype Slug = Slug {unSlug :: Text}
  deriving (Eq, Ord, Show)

data Section
  = Posts
  | Slides
  deriving (Eq, Ord, Show)

data ContentRef = ContentRef
  { contentSection :: Section,
    contentSlug :: Slug
  }
  deriving (Eq, Ord, Show)

data LocalizedRef = LocalizedRef
  { localizedLang :: LangCode,
    localizedContent :: ContentRef
  }
  deriving (Eq, Ord, Show)

data RenderFeatures = RenderFeatures
  { renderMath :: Bool,
    renderMermaid :: Bool,
    renderToc :: Bool
  }
  deriving (Eq, Show)

langCodeString :: LangCode -> String
langCodeString = T.unpack . unLangCode

slugString :: Slug -> String
slugString = T.unpack . unSlug

sectionPathSegment :: Section -> Text
sectionPathSegment Posts = "posts"
sectionPathSegment Slides = "slides"

sectionPathString :: Section -> String
sectionPathString = T.unpack . sectionPathSegment

sectionFromPathSegment :: Text -> Maybe Section
sectionFromPathSegment "posts" = Just Posts
sectionFromPathSegment "slides" = Just Slides
sectionFromPathSegment _ = Nothing
```

- [ ] **Step 2: Expose `Content.Types` in `package.yaml`**

In `package.yaml`, add this entry under `library.exposed-modules`:

```yaml
- Content.Types
```

- [ ] **Step 3: Expose `Content.Types` in `builder.cabal`**

In `builder.cabal`, add this entry under `library exposed-modules`:

```cabal
      Content.Types
```

- [ ] **Step 4: Run the build**

Run:

```bash
nix build
```

Expected: build succeeds with no warnings promoted to errors.

- [ ] **Step 5: Commit checkpoint if authorized**

Run only if commits were explicitly authorized:

```bash
git add src/Content/Types.hs package.yaml builder.cabal
git commit -m "refactor: add typed content primitives"
```

---

### Task 3: Add centralized route construction and parsing

**Files:**

- Create: `src/Routes.hs`
- Create: `test/RoutesSpec.hs`
- Modify: `test/Spec.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Write failing route tests**

Create `test/RoutesSpec.hs` with this complete test module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RoutesSpec (spec) where

import Content.Types
import Data.Either (isLeft)
import Hakyll (fromFilePath, toFilePath)
import Routes
import Test.Hspec

spec :: Spec
spec = describe "Routes" $ do
  describe "public URLs" $ do
    it "renders home URLs" $ do
      homeUrl (LangCode "en") `shouldBe` "/en/"
      homeUrl (LangCode "zh") `shouldBe` "/zh/"

    it "renders post URLs" $
      postUrl (LangCode "en") (Slug "hello-world") `shouldBe` "/en/posts/hello-world/"

    it "renders slide URLs" $
      slideUrl (LangCode "zh") (Slug "nix-intro") `shouldBe` "/zh/slides/nix-intro/"

    it "renders feed URLs" $
      feedUrl (LangCode "en") `shouldBe` "/en/feed.xml"

    it "renders page URLs from path segments" $
      pageUrl (LangCode "zh") ["icpc", "templates", "math"] `shouldBe` "/zh/icpc/templates/math/"

  describe "output paths" $ do
    it "renders post output paths" $
      postOutputPath (LangCode "en") (Slug "hello-world") `shouldBe` "en/posts/hello-world/index.html"

    it "renders slide output paths" $
      slideOutputPath (LangCode "zh") (Slug "nix-intro") `shouldBe` "zh/slides/nix-intro/index.html"

    it "renders section page identifiers" $ do
      toFilePath (sectionPageIdentifier (LangCode "en") Posts 1) `shouldBe` "en/posts/index.html"
      toFilePath (sectionPageIdentifier (LangCode "en") Posts 2) `shouldBe` "en/posts/page/2/index.html"
      toFilePath (sectionPageIdentifier (LangCode "zh") Slides 5) `shouldBe` "zh/slides/page/5/index.html"

  describe "identifier parsing" $ do
    it "extracts language from localized index files" $ do
      langFromIndexIdentifier (fromFilePath "content/posts/hello-world/index.en.md") `shouldBe` Right (LangCode "en")
      langFromIndexIdentifier (fromFilePath "content/about/index.zh.md") `shouldBe` Right (LangCode "zh")

    it "rejects files that are not localized index markdown" $
      langFromIndexIdentifier (fromFilePath "content/slides/nix-intro/slides.md") `shouldSatisfy` isLeft

    it "extracts post slugs" $
      postSlugFromIdentifier (fromFilePath "content/posts/hello-world/index.zh.md") `shouldBe` Right (Slug "hello-world")

    it "extracts slide slugs" $
      slideSlugFromIdentifier (fromFilePath "content/slides/nix-intro/slides.md") `shouldBe` Right (Slug "nix-intro")

    it "rejects malformed post identifiers" $
      postSlugFromIdentifier (fromFilePath "content/posts/index.zh.md") `shouldSatisfy` isLeft

    it "extracts standalone page path segments" $ do
      pageSegmentsFromIdentifier (fromFilePath "content/about/index.en.md") `shouldBe` Right ["about"]
      pageSegmentsFromIdentifier (fromFilePath "content/icpc/templates/math/index.zh.md") `shouldBe` Right ["icpc", "templates", "math"]
```

- [ ] **Step 2: Register `RoutesSpec`**

Modify `test/Spec.hs` to include `RoutesSpec`:

```haskell
module Main (main) where

import qualified ConfigSpec
import qualified PaginateSpec
import qualified RoutesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ConfigSpec.spec
  PaginateSpec.spec
  RoutesSpec.spec
```

- [ ] **Step 3: Run tests to verify failure**

Run:

```bash
nix flake check
```

Expected: check fails because module `Routes` does not exist.

- [ ] **Step 4: Create `Routes.hs`**

Create `src/Routes.hs` with this complete module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( RouteError (..),
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

newtype RouteError = RouteError {unRouteError :: String}
  deriving (Eq, Show)

routeErrorMessage :: RouteError -> String
routeErrorMessage = unRouteError

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

contentAssetOutputPath :: Section -> LangCode -> Slug -> FilePath -> FilePath
contentAssetOutputPath section lang slug fileName =
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
  let withoutMarkdownExtension = dropExtension fileName
      langExtension = takeExtension withoutMarkdownExtension
      baseName = dropExtension withoutMarkdownExtension
      langCode = drop 1 langExtension
   in case (baseName, langCode, takeExtension fileName) of
        ("index", code@(_ : _), ".md") -> Right $ LangCode $ T.pack code
        _ -> Left $ RouteError $ "Expected index.<lang>.md, got: " <> fileName
```

- [ ] **Step 5: Expose `Routes` in package files**

Add `Routes` to `library.exposed-modules` in both `package.yaml` and `builder.cabal`.

- [ ] **Step 6: Run tests and build**

Run:

```bash
nix flake check
```

Expected: tests pass, formatting and lint checks pass, and build check passes.

- [ ] **Step 7: Commit checkpoint if authorized**

Run only if commits were explicitly authorized:

```bash
git add src/Routes.hs test/RoutesSpec.hs test/Spec.hs package.yaml builder.cabal
git commit -m "refactor: centralize route construction"
```

---

### Task 4: Convert configuration languages to `LangCode`

**Files:**

- Modify: `src/Config.hs`
- Modify: `test/ConfigSpec.hs`

- [ ] **Step 1: Update config tests first**

In `test/ConfigSpec.hs`, import `Content.Types` and replace all test language constructors with `LangCode`:

```haskell
import Content.Types (LangCode (..))
```

Use this form wherever a language is constructed:

```haskell
let langs = [Language (LangCode "en") "English", Language (LangCode "zh") "中文"]
```

Update translation calls from string language arguments to typed language arguments:

```haskell
getTrans langs (LangCode "en") translated `shouldBe` "Hello"
getTrans langs (LangCode "zh") translated `shouldBe` "你好"
getTransList langs (LangCode "en") phrases `shouldBe` ["Hi", "Hello"]
getTransList langs (LangCode "zh") phrases `shouldBe` ["嗨", "你好"]
defaultLang langs `shouldBe` LangCode "zh"
defaultLang [] `shouldBe` LangCode "en"
```

Update `minimalConfig` language construction:

```haskell
languages = [Language (LangCode "en") "English"],
```

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
nix flake check
```

Expected: check fails because `Config.Language.langCode` is still `Text` and translation functions still accept `String`.

This task intentionally starts a typed-language migration that may not be build-green until downstream call sites are converted in later tasks. Do not stop or claim the repository is complete after this task; continue through the remaining conversion tasks and verify with `nix build`/`nix flake check` once all call sites have been updated.

- [ ] **Step 3: Update `Config.hs` exports and imports**

In `src/Config.hs`, import `Content.Types`:

```haskell
import Content.Types (LangCode (..))
```

Keep `langCodes` exported, but change its type to `[LangCode]`.

- [ ] **Step 4: Change translation function signatures**

Replace the existing signatures and implementations with typed versions:

```haskell
getTrans :: [Language] -> LangCode -> Translated -> Text
getTrans langs lang (Translated m) =
  let langText = unLangCode lang
      defLang = unLangCode $ defaultLang langs
   in case Map.lookup langText m of
        Just t -> t
        Nothing -> Map.findWithDefault "" defLang m

transStr :: [Language] -> LangCode -> Translated -> String
transStr langs lang = T.unpack . getTrans langs lang

getTransList :: [Language] -> LangCode -> TranslatedList -> [Text]
getTransList langs lang (TranslatedList m) =
  let langText = unLangCode lang
      defLang = unLangCode $ defaultLang langs
   in case Map.lookup langText m of
        Just ts -> ts
        Nothing -> Map.findWithDefault [] defLang m

defaultLang :: [Language] -> LangCode
defaultLang [] = LangCode "en"
defaultLang (l : _) = langCode l

langCodes :: SiteConfig -> [LangCode]
langCodes = map langCode . languages
```

- [ ] **Step 5: Change `Language` and its parser**

Replace `Language` and its `FromJSON` instance with:

```haskell
data Language = Language
  { langCode :: LangCode,
    langLabel :: Text
  }
  deriving (Show, Generic)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \v ->
    Language <$> (LangCode <$> v .: "code") <*> v .: "label"
```

- [ ] **Step 6: Run tests and build**

Run:

```bash
nix flake check
```

Expected: failures now point to call sites outside `ConfigSpec` that still pass `String` languages.

- [ ] **Step 7: Do not patch all call sites in this task**

Leave non-config call site failures for Task 7 through Task 12, where each subsystem will be converted with its owning module.

- [ ] **Step 8: Commit checkpoint if authorized after the full tree builds later**

Do not commit this task independently if the repository does not build at this point.

---

### Task 5: Convert pagination to route-backed typed sections

**Files:**

- Modify: `src/Paginate.hs`
- Modify: `test/PaginateSpec.hs`

- [ ] **Step 1: Update `PaginateSpec` first**

Replace `test/PaginateSpec.hs` with:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module PaginateSpec (spec) where

import Content.Types
import Hakyll (toFilePath)
import Paginate
import Test.Hspec

spec :: Spec
spec = describe "Paginate" $
  describe "makePageId" $ do
    it "generates clean URL for page 1" $ do
      toFilePath (makePageId (LangCode "en") Posts 1) `shouldBe` "en/posts/index.html"
      toFilePath (makePageId (LangCode "zh") Posts 1) `shouldBe` "zh/posts/index.html"

    it "generates paginated URL for page 2+" $ do
      toFilePath (makePageId (LangCode "en") Posts 2) `shouldBe` "en/posts/page/2/index.html"
      toFilePath (makePageId (LangCode "en") Posts 3) `shouldBe` "en/posts/page/3/index.html"
      toFilePath (makePageId (LangCode "zh") Slides 5) `shouldBe` "zh/slides/page/5/index.html"

    it "works with different sections" $ do
      toFilePath (makePageId (LangCode "en") Slides 1) `shouldBe` "en/slides/index.html"
      toFilePath (makePageId (LangCode "en") Slides 2) `shouldBe` "en/slides/page/2/index.html"
```

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
nix flake check
```

Expected: check fails because `makePageId` still takes `String -> String -> PageNumber`.

- [ ] **Step 3: Update `Paginate.hs`**

Replace the `makePageId` implementation and imports with:

```haskell
import Content.Types (LangCode, Section)
import qualified Data.Map as Map
import Hakyll
import Routes (sectionPageIdentifier)

makePageId :: LangCode -> Section -> PageNumber -> Identifier
makePageId = sectionPageIdentifier
```

Keep `paginationCtx` unchanged.

- [ ] **Step 4: Run tests and build**

Run:

```bash
nix flake check
```

Expected: remaining failures are call sites in `Site.hs` or future rule modules still passing strings into `makePageId`.

---

### Task 6: Centralize metadata parsing

**Files:**

- Create: `src/Content/Metadata.hs`
- Create: `test/MetadataSpec.hs`
- Modify: `test/Spec.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Write metadata tests**

Create `test/MetadataSpec.hs` with this complete test module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module MetadataSpec (spec) where

import Content.Metadata
import Content.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Key (fromString)
import Data.Aeson (Value (..))
import Hakyll (Metadata)
import Test.Hspec

spec :: Spec
spec = describe "Content.Metadata" $ do
  describe "metadataBool" $ do
    it "accepts true case-insensitively" $ do
      metadataBool "math" (metadata [("math", String "true")]) `shouldBe` True
      metadataBool "math" (metadata [("math", String "TRUE")]) `shouldBe` True

    it "defaults missing values to false" $
      metadataBool "math" (metadata []) `shouldBe` False

  describe "featuresFromMetadata" $
    it "reads render feature flags" $
      featuresFromMetadata (metadata [("math", String "true"), ("mermaid", String "true"), ("toc", String "false")])
        `shouldBe` RenderFeatures True True False

  describe "templateFromMetadata" $
    it "defaults to page" $
      templateFromMetadata (metadata []) `shouldBe` "page"

  describe "slideLevelFromMetadata" $
    it "defaults to 2" $
      slideLevelFromMetadata (metadata []) `shouldBe` 2

metadata :: [(String, Value)] -> Metadata
metadata = KeyMap.fromList . map (\(key, value) -> (fromString key, value))
```

- [ ] **Step 2: Register `MetadataSpec`**

Modify `test/Spec.hs` to include `MetadataSpec`:

```haskell
module Main (main) where

import qualified ConfigSpec
import qualified MetadataSpec
import qualified PaginateSpec
import qualified RoutesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ConfigSpec.spec
  MetadataSpec.spec
  PaginateSpec.spec
  RoutesSpec.spec
```

- [ ] **Step 3: Run tests to verify failure**

Run:

```bash
nix flake check
```

Expected: check fails because module `Content.Metadata` does not exist.

- [ ] **Step 4: Create `Content.Metadata`**

Create `src/Content/Metadata.hs` with this complete module:

```haskell
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

metadataBool :: String -> Metadata -> Bool
metadataBool key metadata = maybe False ((== "true") . map toLower) $ lookupString key metadata

metadataString :: String -> Metadata -> Maybe String
metadataString = lookupString

metadataStringOrEmpty :: String -> Metadata -> String
metadataStringOrEmpty key = fromMaybe "" . metadataString key

metadataStringList :: String -> Metadata -> [String]
metadataStringList key metadata = fromMaybe [] $ lookupStringList key metadata

featuresFromMetadata :: Metadata -> RenderFeatures
featuresFromMetadata metadata =
  RenderFeatures
    { renderMath = metadataBool "math" metadata,
      renderMermaid = metadataBool "mermaid" metadata,
      renderToc = metadataBool "toc" metadata
    }

templateFromMetadata :: Metadata -> String
templateFromMetadata metadata = fromMaybe "page" $ metadataString "template" metadata

slideLevelFromMetadata :: Metadata -> Int
slideLevelFromMetadata metadata = fromMaybe 2 $ metadataString "slide-level" metadata >>= readMaybe
```

- [ ] **Step 5: Expose `Content.Metadata` in package files**

Add `Content.Metadata` to `library.exposed-modules` in both `package.yaml` and `builder.cabal`.

Also add `aeson` to the `tests.builder-test.dependencies` list in `package.yaml` and to the `test-suite builder-test build-depends` list in `builder.cabal`, because `MetadataSpec` constructs Hakyll `Metadata` values through Aeson `Value`/`KeyMap`.

- [ ] **Step 6: Run tests and build**

Run:

```bash
nix flake check
```

Expected: metadata tests pass. Remaining failures can still exist if earlier typed language changes are incomplete.

---

### Task 7: Move context ownership into `Context.hs`

**Files:**

- Modify: `src/Context.hs`
- Modify: `src/Site.hs`

- [ ] **Step 1: Add typed context imports**

In `src/Context.hs`, add imports:

```haskell
import Content.Metadata (metadataStringList)
import Content.Types (LangCode, langCodeString)
import Routes (postUrl)
```

- [ ] **Step 2: Change core context signatures to typed languages**

Change these signatures:

```haskell
baseCtx :: SiteConfig -> LangCode -> Context String
langCtx :: LangCode -> Context String
homeCtx :: SiteConfig -> LangCode -> Context String
tocCtx :: LangCode -> Maybe String -> Context String
postTagsField :: LangCode -> Context String
tagsFieldFromMeta :: LangCode -> Metadata -> Context String
navCtx :: SiteConfig -> LangCode -> Context String
socialCtx :: SiteConfig -> LangCode -> Context String
```

Where `constField "lang"` is used, call `langCodeString`:

```haskell
langCtx :: LangCode -> Context String
langCtx = constField "lang" . langCodeString
```

- [ ] **Step 3: Move context helpers from `Site.hs` into `Context.hs`**

Move these complete definitions from `src/Site.hs` into `src/Context.hs`, adapting language arguments to `LangCode`:

```haskell
postCtx :: SiteConfig -> LangCode -> Context String
postCtx cfg lang = postTagsField lang <> baseCtx cfg lang <> postMetaCtx

pageCtx :: SiteConfig -> LangCode -> String -> Context String
pageCtx cfg lang "about" = baseCtx cfg lang <> friendsCtx cfg
pageCtx cfg lang _ = baseCtx cfg lang

postListItemCtx :: LangCode -> Context String
postListItemCtx lang = field "url" makeUrl <> field "tagsStr" getTagsStr <> dateCtx <> defaultContext
  where
    makeUrl item = routeOrFail $ postUrl lang <$> postSlugFromIdentifier (itemIdentifier item)
    getTagsStr item = do
      meta <- getMetadata (itemIdentifier item)
      pure $ intercalate "," $ metadataStringList "tags" meta

allLangsCtx :: SiteConfig -> LangCode -> (LangCode -> String) -> Context String
allLangsCtx cfg curLang makeUrl =
  availableLangsCtx
    [ AvailableLang (langCodeString $ langCode l) (T.unpack $ langLabel l) (makeUrl $ langCode l) (langCode l == curLang)
    | l <- languages cfg
    ]

navTitle :: SiteConfig -> LangCode -> T.Text -> String
navTitle cfg lang url = case filter ((== url) . navUrl) $ navigation cfg of
  (n : _) -> T.unpack $ getTrans (languages cfg) lang $ navLabel n
  [] -> T.unpack url
```

Also import these route helpers in `Context.hs`:

```haskell
import Routes (postSlugFromIdentifier, postUrl, routeOrFail)
```

- [ ] **Step 4: Export moved context helpers**

Add these names to the `Context` export list:

```haskell
postCtx,
pageCtx,
postListItemCtx,
allLangsCtx,
navTitle,
```

- [ ] **Step 5: Remove moved context helpers from `Site.hs`**

Delete these definitions from `src/Site.hs` after all call sites compile against `Context.hs`:

```text
postCtx
pageCtx
postListItemCtx
allLangsCtx
navTitle
```

- [ ] **Step 6: Run build**

Run:

```bash
nix build
```

Expected: compiler errors identify remaining `String` language call sites. Fix them by wrapping string values with `LangCode` only at Hakyll boundaries, or by carrying `LangCode` from `Config.Language.langCode`.

---

### Task 8: Extract static and asset rules

**Files:**

- Create: `src/Rules/Static.hs`
- Modify: `src/Site.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Create `Rules.Static`**

Create `src/Rules/Static.hs` and move these definitions from `src/Site.hs` unchanged except for imports and route helper usage:

```text
imageExtensions
staticFiles
contentAssets
scssCompilation
syntaxHighlightingCss
syntaxCssWithThemes
bgColorCss
fgColorCss
extractTokenStyles
templates
errorPage
```

Use this module header:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Rules.Static
  ( staticRules,
    contentAssetRules,
  )
where
```

Expose these functions:

```haskell
staticRules :: Rules ()
staticRules = do
  staticFiles
  scssCompilation
  syntaxHighlightingCss
  templates
  errorPage

contentAssetRules :: SiteConfig -> Rules ()
contentAssetRules = contentAssets
```

- [ ] **Step 2: Replace asset route construction**

In `contentAssets`, replace manual path construction with `contentAssetOutputPath`:

```haskell
contentAssets :: SiteConfig -> Rules ()
contentAssets cfg =
  forM_ imageExtensions $ \ext ->
    forM_ [(Posts, "posts"), (Slides, "slides")] $ \(sectionValue, sectionDir) ->
      match (fromGlob $ "content/" <> sectionDir <> "/*/*." <> ext) $
        forM_ (languages cfg) $ \lang ->
          version (langCodeString $ langCode lang) $ do
            route $ customRoute $ \identifier ->
              case slugFromSectionIdentifier sectionValue identifier of
                Right slug -> contentAssetOutputPath sectionValue (langCode lang) slug (takeFileName $ toFilePath identifier)
                Left err -> error $ routeErrorMessage err
            compile copyFileCompiler
```

Use `fail` inside compiler code and `error` only in `customRoute`, where Hakyll requires a pure route function.

- [ ] **Step 3: Wire `Rules.Static` in `Site.hs`**

In `src/Site.hs`, import:

```haskell
import Rules.Static (contentAssetRules, staticRules)
```

Replace direct static calls in `rules` with:

```haskell
staticRules
```

Replace `contentAssets cfg` with:

```haskell
contentAssetRules cfg
```

- [ ] **Step 4: Expose `Rules.Static` in package files**

Add `Rules.Static` to `library.exposed-modules` in both `package.yaml` and `builder.cabal`.

- [ ] **Step 5: Run build**

Run:

```bash
nix build
```

Expected: build succeeds and static files still route to the same output paths.

---

### Task 9: Extract post fallback and post rules

**Files:**

- Create: `src/Content/Fallback.hs`
- Create: `src/Rules/Posts.hs`
- Modify: `src/Site.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Create `Content.Fallback`**

Create `src/Content/Fallback.hs` with:

```haskell
module Content.Fallback
  ( preferredLangOrder,
  )
where

import Config (Language (..), SiteConfig (..))
import Content.Types (LangCode)

preferredLangOrder :: SiteConfig -> LangCode -> [LangCode]
preferredLangOrder cfg requested = requested : filter (/= requested) (map langCode $ languages cfg)
```

- [ ] **Step 2: Create `Rules.Posts` module shell**

Create `src/Rules/Posts.hs` with this export list:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Rules.Posts
  ( postRules,
    postListRules,
    loadAllPostsForLang,
  )
where
```

- [ ] **Step 3: Move post functions from `Site.hs`**

Move these complete definitions from `src/Site.hs` into `src/Rules/Posts.hs`:

```text
blogPosts
createFallbackPost
loadAllPostsForLang
findPostIdent
bestPostForLang
nubOrd
```

Rename exported rule functions:

```haskell
postRules :: SiteConfig -> Rules ()
postRules = blogPosts

postListRules :: SiteConfig -> Rules ()
postListRules = postList
```

Also move `postList` into `Rules.Posts` because it depends on `loadAllPostsForLang` and `groupByYear`.

- [ ] **Step 4: Convert post code to typed routes**

Replace these old operations:

```text
extractLang identifier
slugFromPath identifier
targetLang </> "posts" </> slug </> "index.html"
"/posts/" <> slug <> "/"
```

with these route operations:

```haskell
lang <- routeOrFail $ langFromIndexIdentifier identifier
slug <- routeOrFail $ postSlugFromIdentifier identifier
postOutputPath lang slug
postUrl lang slug
```

Where a Hakyll `customRoute` requires pure code, convert `Either RouteError FilePath` using `either (error . routeErrorMessage) id`.

- [ ] **Step 5: Preserve fallback semantics explicitly**

In `createFallbackPost`, name the two language roles clearly:

```haskell
createFallbackPost :: SiteConfig -> LangCode -> Slug -> Rules ()
createFallbackPost cfg requestedLang slug =
  create [fromFilePath $ postOutputPath requestedLang slug] $ do
    route idRoute
    compile $ do
      maybeSourceIdentifier <- findPostIdent cfg requestedLang slug
      case maybeSourceIdentifier of
        Nothing -> fail $ "No source found for post: " <> slugString slug
        Just sourceIdentifier -> renderFallbackPost cfg requestedLang slug sourceIdentifier
```

Keep the existing behavior that source body and metadata come from `sourceIdentifier`, while navigation, language switcher, and route fields use `requestedLang`.

- [ ] **Step 6: Preserve snapshot names**

Keep these exact strings:

```haskell
saveSnapshot "content"
saveSnapshot "toc"
loadSnapshot sourceIdentifier "content"
loadSnapshot sourceIdentifier "toc"
```

- [ ] **Step 7: Wire posts in `Site.hs`**

Import:

```haskell
import Rules.Posts (postListRules, postRules)
```

Replace:

```haskell
blogPosts cfg
postList cfg
```

with:

```haskell
postRules cfg
postListRules cfg
```

- [ ] **Step 8: Expose modules in package files**

Add these modules to `library.exposed-modules` in both `package.yaml` and `builder.cabal`:

```text
Content.Fallback
Rules.Posts
```

- [ ] **Step 9: Run build**

Run:

```bash
nix build
```

Expected: build succeeds. Inspect generated files under `result/en/posts/hello-world/index.html` and `result/zh/posts/hello-world/index.html` if symlink exists after the build.

---

### Task 10: Extract page rules

**Files:**

- Create: `src/Rules/Pages.hs`
- Modify: `src/Site.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Create `Rules.Pages`**

Create `src/Rules/Pages.hs` with this export list:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Rules.Pages
  ( pageRules,
  )
where
```

- [ ] **Step 2: Move page rules from `Site.hs`**

Move these definitions from `src/Site.hs` into `Rules.Pages`:

```text
homepages
standalonePages
nestedPageLangsCtx
```

Expose them through:

```haskell
pageRules :: SiteConfig -> Rules ()
pageRules cfg = do
  homepages cfg
  standalonePages cfg
```

- [ ] **Step 3: Convert page routing to `Routes`**

For homepage routing, use:

```haskell
route $ customRoute $ either (error . routeErrorMessage) homeOutputPath . langFromIndexIdentifier
```

For standalone page routing, use:

```haskell
route $ customRoute $ \identifier ->
  case (langFromIndexIdentifier identifier, pageSegmentsFromIdentifier identifier) of
    (Right lang, Right segments) -> pageOutputPath lang segments
    (Left err, _) -> error $ routeErrorMessage err
    (_, Left err) -> error $ routeErrorMessage err
```

- [ ] **Step 4: Add fallback page rules for missing page translations**

Standalone pages must follow the same invariant as posts: every page path is reachable at every configured language URL. After moving the current direct page compilation, add fallback creation for missing page translations in `Rules.Pages`.

Use this structure:

```haskell
standalonePages :: SiteConfig -> Rules ()
standalonePages cfg = do
  match standalonePagePattern $ do
    route $ customRoute pageRoute
    compile $ do
      identifier <- getUnderlying
      requestedLang <- routeOrFail $ langFromIndexIdentifier identifier
      compileStandalonePage cfg requestedLang identifier

  pageDep <- makePatternDependency standalonePagePattern
  rulesExtraDependencies [pageDep] $ do
    pageFiles <- getMatches standalonePagePattern
    let pageRefs = nubOrd $ rights $ map pageSegmentsFromIdentifier pageFiles
    forM_ pageRefs $ \segments ->
      forM_ (languages cfg) $ \lang ->
        unless (hasPageTranslation (langCode lang) segments pageFiles) $
          createFallbackPage cfg (langCode lang) segments
```

Define the helper responsibilities explicitly:

```haskell
standalonePagePattern :: Pattern
standalonePagePattern = "content/**/index.*.md" .&&. complement "content/index.*.md" .&&. complement "content/posts/**"

compileStandalonePage :: SiteConfig -> LangCode -> Identifier -> Compiler (Item String)
```

`compileStandalonePage` must read metadata from the source identifier, use the requested language for context/navigation, save the rendered body to snapshot `"content"`, apply the selected page template, apply `templates/default.html`, and relativize URLs.

`createFallbackPage` must choose the best source identifier with requested language first and configured-language fallback order after that. It must load the source page snapshot `"content"`, reuse the source metadata for title/template/render flags, and render using the requested language context and requested language URL.

`hasPageTranslation` must compare both `LangCode` and path segments; it must not rely on filename string slicing outside `Routes` helpers.

- [ ] **Step 5: Wire pages in `Site.hs`**

Import:

```haskell
import Rules.Pages (pageRules)
```

Replace:

```haskell
homepages cfg
standalonePages cfg
```

with:

```haskell
pageRules cfg
```

- [ ] **Step 6: Expose `Rules.Pages` in package files**

Add `Rules.Pages` to `library.exposed-modules` in both `package.yaml` and `builder.cabal`.

- [ ] **Step 7: Run build**

Run:

```bash
nix build
```

Expected: build succeeds and `/en/about/` plus `/zh/about/` still exist in the output.

---

### Task 11: Extract slide rules

**Files:**

- Create: `src/Rules/Slides.hs`
- Modify: `src/Site.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Create `Rules.Slides`**

Create `src/Rules/Slides.hs` with:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Rules.Slides
  ( slideRules,
  )
where
```

- [ ] **Step 2: Move slide rules from `Site.hs`**

Move these definitions from `src/Site.hs` into `Rules.Slides`:

```text
slidePages
slideList
```

Expose them through:

```haskell
slideRules :: SiteConfig -> Rules ()
slideRules cfg = do
  slidePages cfg
  slideList cfg
```

- [ ] **Step 3: Convert slide routing to `Routes`**

Replace manual slide output paths with:

```haskell
slideOutputPath (langCode lang) slug
```

Replace list item URLs with:

```haskell
slideUrl lang slug
```

Use `slideSlugFromIdentifier` and `routeOrFail` to parse source identifiers.

- [ ] **Step 4: Convert slide pagination to typed `makePageId`**

Replace:

```haskell
makePageId ls "slides"
```

with:

```haskell
makePageId (langCode lang) Slides
```

- [ ] **Step 5: Wire slides in `Site.hs`**

Import:

```haskell
import Rules.Slides (slideRules)
```

Replace:

```haskell
slidePages cfg
slideList cfg
```

with:

```haskell
slideRules cfg
```

- [ ] **Step 6: Expose `Rules.Slides` in package files**

Add `Rules.Slides` to `library.exposed-modules` in both `package.yaml` and `builder.cabal`.

- [ ] **Step 7: Run build**

Run:

```bash
nix build
```

Expected: build succeeds and slide list routes still render.

---

### Task 12: Extract feed and sitemap rules

**Files:**

- Create: `src/Rules/Feeds.hs`
- Create: `src/Rules/Sitemap.hs`
- Modify: `src/Feed.hs`
- Modify: `src/Site.hs`
- Modify: `package.yaml`
- Modify: `builder.cabal`

- [ ] **Step 1: Update `Feed.hs` to stop parsing slugs manually**

Remove `System.FilePath (splitDirectories)` import from `Feed.hs`.

Import:

```haskell
import Content.Types (LangCode)
import Routes (postSlugFromIdentifier, postUrl, routeOrFail)
```

Change signatures:

```haskell
feedConfiguration :: SiteConfig -> LangCode -> FeedConfiguration
feedCtxForLang :: LangCode -> Context String
urlFieldForLang :: LangCode -> Context a
```

Replace `urlFieldForLang` body with:

```haskell
urlFieldForLang :: LangCode -> Context a
urlFieldForLang targetLang = field "url" $ \item -> do
  slug <- routeOrFail $ postSlugFromIdentifier $ itemIdentifier item
  pure $ postUrl targetLang slug
```

- [ ] **Step 2: Create `Rules.Feeds`**

Create `src/Rules/Feeds.hs` and move `rssFeeds` from `Site.hs` into it. Export:

```haskell
module Rules.Feeds
  ( feedRules,
  )
where
```

Use:

```haskell
feedRules :: SiteConfig -> Rules ()
feedRules = rssFeeds
```

Replace feed output path creation with:

```haskell
create [fromFilePath $ feedOutputPath $ langCode lang]
```

- [ ] **Step 3: Create `Rules.Sitemap`**

Create `src/Rules/Sitemap.hs` and move `SitemapEntry` plus `sitemap` from `Site.hs` into it. Export:

```haskell
module Rules.Sitemap
  ( sitemapRules,
  )
where
```

Use route helpers for every URL:

```haskell
sitemapRules :: SiteConfig -> Rules ()
sitemapRules = sitemap
```

Build sitemap locations with:

```haskell
absoluteUrl root relativeUrl = root <> relativeUrl
postUrl lang slug
slideUrl lang slug
pageUrl lang ["about"]
sectionIndexUrl lang Posts
sectionIndexUrl lang Slides
```

- [ ] **Step 4: Wire feeds and sitemap in `Site.hs`**

Import:

```haskell
import Rules.Feeds (feedRules)
import Rules.Sitemap (sitemapRules)
```

Replace:

```haskell
rssFeeds cfg
sitemap cfg
```

with:

```haskell
feedRules cfg
sitemapRules cfg
```

- [ ] **Step 5: Expose new rule modules in package files**

Add these to `library.exposed-modules` in both `package.yaml` and `builder.cabal`:

```text
Rules.Feeds
Rules.Sitemap
```

- [ ] **Step 6: Run build**

Run:

```bash
nix build
```

Expected: build succeeds and `result/sitemap.xml`, `result/en/feed.xml`, and `result/zh/feed.xml` exist.

---

### Task 13: Reduce `Site.hs` to composition root and remove old helpers

**Files:**

- Modify: `src/Site.hs`
- Modify: `src/Rules/Static.hs`
- Modify: `src/Rules/Pages.hs`
- Modify: `src/Rules/Posts.hs`
- Modify: `src/Rules/Slides.hs`
- Modify: `src/Rules/Feeds.hs`
- Modify: `src/Rules/Sitemap.hs`

- [ ] **Step 1: Replace `Site.hs` with composition-only structure**

After all rule modules compile, `src/Site.hs` should contain only this shape:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Site (hakyllMain) where

import Config (loadConfig)
import Hakyll
import Rules.Feeds (feedRules)
import Rules.Pages (pageRules)
import Rules.Posts (postListRules, postRules)
import Rules.Sitemap (sitemapRules)
import Rules.Slides (slideRules)
import Rules.Static (contentAssetRules, staticRules)

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "_site",
      storeDirectory = "_cache",
      tmpDirectory = "_cache/tmp",
      providerDirectory = "."
    }

hakyllMain :: IO ()
hakyllMain = hakyllWith config rules

rules :: Rules ()
rules = do
  cfg <- preprocess $ loadConfig "config.yaml"
  configDep <- makePatternDependency "config.yaml"

  staticRules

  rulesExtraDependencies [configDep] $ do
    contentAssetRules cfg
    pageRules cfg
    postRules cfg
    postListRules cfg
    feedRules cfg
    sitemapRules cfg
    slideRules cfg
```

- [ ] **Step 2: Delete these definitions from `Site.hs`**

Verify `Site.hs` no longer defines:

```text
imageExtensions
staticFiles
contentAssets
scssCompilation
syntaxHighlightingCss
syntaxCssWithThemes
bgColorCss
fgColorCss
extractTokenStyles
templates
errorPage
homepages
standalonePages
blogPosts
createFallbackPost
loadAllPostsForLang
postList
rssFeeds
sitemap
slidePages
slideList
postCtx
pageCtx
postListItemCtx
allLangsCtx
nestedPageLangsCtx
extractLang
currentLang
slugFromPath
safeInit
nubOrd
langStr
getFeatureFlags
getTocFlag
navTitle
findPostIdent
bestPostForLang
groupByYear
metaStr
metaBool
boolCtx
```

- [ ] **Step 3: Search for forbidden route duplication**

Run:

```bash
rg '"unknown"|slugFromPath|extractLang|/posts/|/slides/' src
```

Expected: no `"unknown"`, no `slugFromPath`, no `extractLang`. Literal `/posts/` and `/slides/` should appear only in `Routes.hs`, templates, and route tests. If they appear in rule modules, replace them with route helpers.

- [ ] **Step 4: Run full verification**

Run:

```bash
nix flake check
```

Expected: all checks pass.

- [ ] **Step 5: Commit checkpoint if authorized**

Run only if commits were explicitly authorized:

```bash
git add src test package.yaml builder.cabal
git commit -m "refactor: split Hakyll rules by domain"
```

---

### Task 14: Final generated-site smoke checks

**Files:**

- No source changes expected

- [ ] **Step 1: Build the site**

Run:

```bash
nix build
```

Expected: build succeeds and `result` points to the generated site output.

- [ ] **Step 2: Verify core language entry points**

Run:

```bash
test -f result/en/index.html && test -f result/zh/index.html
```

Expected: command exits with code 0.

- [ ] **Step 3: Verify post fallback route exists in both languages**

Run:

```bash
test -f result/en/posts/hello-world/index.html && test -f result/zh/posts/hello-world/index.html
```

Expected: command exits with code 0.

- [ ] **Step 4: Verify feeds and sitemap exist**

Run:

```bash
test -f result/en/feed.xml && test -f result/zh/feed.xml && test -f result/sitemap.xml
```

Expected: command exits with code 0.

- [ ] **Step 5: Verify no placeholder route leaked into output**

Run:

```bash
rg 'unknown' result
```

Expected: command exits with code 1 and prints no matches.

- [ ] **Step 6: Report final architecture state**

Report these facts with evidence from the final diff and verification output:

```text
Site.hs is composition-only.
Routes.hs owns public URLs and output paths.
Content.Types owns language, slug, section, localized content, and render features.
Content.Metadata owns metadata parsing.
Rules modules own Hakyll rule groups.
No route helper returns "unknown".
nix flake check passes.
```

---

## Self-review checklist for the implementation worker

Before claiming the refactor is complete, verify each item:

- [ ] `Site.hs` contains no content-specific helper definitions.
- [ ] `Routes.hs` is the only Haskell module that constructs post and slide URL strings.
- [ ] `Feed.hs` uses `postUrl` and `postSlugFromIdentifier`.
- [ ] Sitemap generation uses route helpers for every entry.
- [ ] Pagination uses `LangCode` and `Section`.
- [ ] Config translation functions accept `LangCode`.
- [ ] Context functions accept `LangCode` except where Hakyll templates require `String` fields.
- [ ] No source file returns `"unknown"` as a route or slug.
- [ ] `nix build` passes.
- [ ] `nix flake check` passes.
- [ ] A final Oracle deep review has approved the integrated refactor.

---

## Execution handoff

Recommended execution mode: **Subagent-Driven**. Dispatch one fresh subagent per task or per pair of tightly coupled tasks, then review the diff and run verification before moving on.

Implementation must proceed only after each batch has passed both review gates:

1. Implement the current batch.
2. Run the required verification for that batch.
3. Run a spec-compliance review.
4. Fix every spec-compliance issue and re-review until approved.
5. Run a code-quality review.
6. Fix every code-quality issue and re-review until approved.
7. Start the next implementation batch only after both reviews approve.

Do not begin the next batch while either review has open issues, even if the build passes.

After all batches pass, run one final holistic code review and one Oracle deep
review of the integrated branch before claiming completion. Fix every blocking
issue found by either reviewer, re-run verification, and re-run the relevant
review until approved. Do not present branch-finishing options while Oracle has
open blocking findings.

Suggested batching:

1. Tasks 1-3: domain types and routes.
2. Tasks 4-6: typed config, pagination, metadata.
3. Tasks 7-8: contexts and static rules.
4. Task 9: posts and fallback.
5. Tasks 10-12: pages, slides, feeds, sitemap.
6. Tasks 13-14: cleanup and final verification.
