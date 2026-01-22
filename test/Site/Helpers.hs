{-# LANGUAGE OverloadedStrings #-}

module Site.Helpers
  ( -- * HTML Parsing
    hasElement,
    hasElementWithClass,
    countElements,
    getAttributeValue,

    -- * File Discovery
    listHtmlFiles,
    listDirectories,
    doesPathExist,

    -- * Path Utilities
    extractSlug,
    isSlidePresentation,
    isSpecialPage,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.HTML.TagSoup (Tag (..))

-- | Check if an element with given tag name exists
hasElement :: T.Text -> [Tag T.Text] -> Bool
hasElement tagName = any isMatch
  where
    isMatch (TagOpen name _) = name == tagName
    isMatch _ = False

-- | Check if an element with given tag and class exists
hasElementWithClass :: T.Text -> T.Text -> [Tag T.Text] -> Bool
hasElementWithClass tagName className = any isMatch
  where
    isMatch (TagOpen name attrs) =
      name == tagName && any (hasClass className) attrs
    isMatch _ = False

    hasClass cls ("class", value) = cls `T.isInfixOf` value
    hasClass _ _ = False

-- | Count elements with given tag name
countElements :: T.Text -> [Tag T.Text] -> Int
countElements tagName = length . filter isMatch
  where
    isMatch (TagOpen name _) = name == tagName
    isMatch _ = False

-- | Get attribute value from first matching element
getAttributeValue :: T.Text -> T.Text -> [Tag T.Text] -> Maybe T.Text
getAttributeValue tagName attrName tags =
  case filter isMatch tags of
    (TagOpen _ attrs : _) -> lookup attrName attrs
    _ -> Nothing
  where
    isMatch (TagOpen name _) = name == tagName
    isMatch _ = False

-- | List all HTML files recursively in a directory
listHtmlFiles :: FilePath -> IO [FilePath]
listHtmlFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else go dir
  where
    go d = do
      entries <- listDirectory d
      paths <- concat <$> mapM (processEntry d) entries
      return paths

    processEntry d entry = do
      let path = d </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then go path
        else
          return $
            if ".html" `isInfixOf` entry
              then [path]
              else []

-- | List directories in a path
listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      filterM (doesDirectoryExist . (dir </>)) entries
  where
    filterM _ [] = return []
    filterM p (x : xs) = do
      keep <- p x
      rest <- filterM p xs
      return $ if keep then x : rest else rest

-- | Check if path exists (file or directory)
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
  isFile <- doesFileExist path
  if isFile
    then return True
    else doesDirectoryExist path

-- | Extract slug from a path like "_site/en/posts/hello-world/index.html"
extractSlug :: FilePath -> String
extractSlug path =
  let parts = filter (not . null) $ splitPath path
   in if length parts >= 4
        then parts !! 3 -- _site / en / posts / slug / index.html
        else ""
  where
    splitPath = foldr splitOn [[]]
    splitOn '/' (x : xs) = [] : (x : xs)
    splitOn c (x : xs) = (c : x) : xs
    splitOn _ [] = []

-- | Check if a path is a slide presentation (not slide list)
isSlidePresentation :: FilePath -> Bool
isSlidePresentation path =
  "/slides/" `isInfixOf` path
    && not ("/slides/index" `isInfixOf` path)
    && not ("/slides/page/" `isInfixOf` path)

-- | Check if a path is a special page (404, etc.) that may have different layout
isSpecialPage :: FilePath -> Bool
isSpecialPage path =
  "/404.html" `isInfixOf` path
    || "/vendor/" `isInfixOf` path -- Vendor files aren't real pages
