module VendorIconsSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

spec :: Spec
spec = describe "vendor icon assets" $ do
  it "uses Tabler webfont assets instead of Lucide" $ do
    flake <- readFile "flake.nix"
    nvfetcher <- readFile "nvfetcher.toml"
    headPartial <- readFile "templates/partials/head.html"

    flake `shouldSatisfy` notContaining "lucide"
    nvfetcher `shouldSatisfy` notContaining "lucide"
    headPartial `shouldSatisfy` notContaining "vendor/lucide"
    headPartial `shouldSatisfy` containing "/vendor/tabler-icons/tabler-icons.min.css"

  it "renders all template icons with Tabler classes" $ do
    home <- readFile "templates/home.html"
    actions <- readFile "templates/partials/actions.html"
    footer <- readFile "templates/partials/footer.html"
    pagination <- readFile "templates/partials/pagination.html"
    slideList <- readFile "templates/slide-list.html"
    theme <- readFile "static/js/theme.js"

    let files = [home, actions, footer, pagination, slideList, theme]

    concat files `shouldSatisfy` notContaining "icon-"
    home `shouldSatisfy` containing "ti ti-$icon$"
    actions `shouldSatisfy` containing "ti ti-list"
    actions `shouldSatisfy` containing "ti ti-sun"
    actions `shouldSatisfy` containing "ti ti-globe"
    footer `shouldSatisfy` containing "ti ti-rss"
    pagination `shouldSatisfy` containing "ti ti-chevron-left"
    pagination `shouldSatisfy` containing "ti ti-chevron-right"
    slideList `shouldSatisfy` containing "ti ti-user"
    slideList `shouldSatisfy` containing "ti ti-calendar"
    theme `shouldSatisfy` containing "ti ti-sun"
    theme `shouldSatisfy` containing "ti ti-moon"

  it "visually truncates slide subtitles in listing cards" $ do
    components <- readFile "static/scss/_components.scss"

    components `shouldSatisfy` containing "&-subtitle"
    components `shouldSatisfy` containing "display: -webkit-box;"
    components `shouldSatisfy` containing "-webkit-line-clamp: 2;"
    components `shouldSatisfy` containing "-webkit-box-orient: vertical;"
    components `shouldSatisfy` containing "overflow: hidden;"

containing :: String -> String -> Bool
containing needle haystack = needle `isInfixOf` haystack

notContaining :: String -> String -> Bool
notContaining needle haystack = not (needle `isInfixOf` haystack)
