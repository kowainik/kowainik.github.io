module Kowainik.StyleGuide
       ( syncStyleGuide
       ) where

import Network.HTTP.Client (httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (ok200)
import System.FilePath ((</>))

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as T


syncStyleGuide :: IO ()
syncStyleGuide = do
    manager <- newTlsManager
    request <- parseRequest "https://raw.githubusercontent.com/kowainik/org/master/style-guide.md"
    response <- httpLbs request manager

    if responseStatus response == ok200 then do
        let responseText = LT.unlines $ drop 1 $ LT.lines $ T.decodeUtf8 $ responseBody response

        let filepath :: FilePath = "posts" </> "2019-02-06-style-guide.md"
        writeFileLText filepath $ createMdHeader <> responseText
    else error "Status code is not ok"
  where
    createMdHeader :: LText
    createMdHeader = toLazy $ unlines
        [ "---"
        , "title: Haskell Style Guide"
        , "author: Kowainik"
        , "tags: haskell, guide"
        , "description: Haskell Style guide used in Kowainik"
        , "---"
        , ""
        ]
