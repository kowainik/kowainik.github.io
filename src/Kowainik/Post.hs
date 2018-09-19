module Kowainik.Post
       ( createPostsWithToc
       ) where

import Data.Char (isAlphaNum, isSpace)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))

import qualified Data.Text as T


-- | Generates posts md with metadata 'toc'.
createPostsWithToc :: IO ()
createPostsWithToc = do
    createDirectoryIfMissing True "posts"
    posts <- listDirectory "posts-raw"
    for_ posts $ \post -> do
        content <- readFileText $ "posts-raw" </> post
        writeFileText ("posts" </> post) $ addToc content

{- | Modifies the body of the file in the following way:

1. Adds meta tag @toc@ with the html for the toc.
2. Changes header name to the link.
   @## Header One@ will become @## [Header One](#header-one)@.

-}
addToc :: Text -> Text
addToc txt = let (tocUl, txtLinks) = walk 0 $ lines txt
                 toc = "toc: \"" <> tocUl <> "\""
                 (meta, rest) = splitAt 4 txtLinks in
    unlines $ meta ++ toc : rest
  where
    walk :: Int    -- ^ number of # minus one
         -> [Text] -- ^ remaining lines
         -> (Text, [Text]) -- ^ toc and updated lines
    walk n [] = (T.replicate n "</ul>", [])
    walk n (x:xs) = if "#" `T.isPrefixOf` x
        then let sharp = T.takeWhile (== '#') x
                 l = T.length sharp
                 name = T.drop (l + 1) x
                 href = T.intercalate "-" $ map T.toLower $ words $ T.filter isHeaderChar name
                 li = "<li><a href='#" <> href <> "'>" <> name <> "</a></li>"
                 newHeader = sharp <> " [" <> name <> "](#" <> href <> ")"
                 newToc = case compare n (l - 1) of
                     EQ -> li
                     LT -> "<ul> " <> li
                     GT -> "</ul> " <> li
                 (fToc, fList) = walk (l - 1) xs
             in (newToc <> fToc, newHeader:fList)
        else let (fToc, fList) = walk n xs in (fToc, x:fList)

    isHeaderChar :: Char -> Bool
    isHeaderChar c = isAlphaNum c || isSpace c || c == '.'
