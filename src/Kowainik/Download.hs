module Kowainik.Download
       ( syncStyleGuide
       , getStanReport
       ) where

import Network.HTTP.Client (httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (ok200)
import System.FilePath ((</>))

import qualified Data.Text.Lazy as LT


downloadFile :: String -> IO LByteString
downloadFile link = do
    manager <- newTlsManager
    request <- parseRequest link
    response <- httpLbs request manager

    if responseStatus response == ok200
    then pure $ responseBody response
    else error "Status code is not ok"

syncStyleGuide :: IO ()
syncStyleGuide = do
    resp <- downloadFile "https://raw.githubusercontent.com/kowainik/org/master/style-guide.md"

    let responseText = LT.unlines $ drop 1 $ LT.lines $ decodeUtf8 resp
    let filepath :: FilePath = "posts" </> "2019-02-06-style-guide.md"
    writeFileLText filepath $ createMdHeader <> responseText
  where
    createMdHeader :: LText
    createMdHeader = toLazy $ unlines
        [ "---"
        , "title: Haskell Style Guide"
        , "author: Kowainik"
        , "tags: haskell, guide"
        , "description: Haskell Style guide used in Kowainik"
        , "updated: \"September 22, 2020\""
        , "---"
        , ""
        ]

getStanReport :: IO String
getStanReport = decodeUtf8 <$> downloadFile "https://raw.githubusercontent.com/kowainik/stan/master/stan.html"
