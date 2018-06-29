module Kowainik.Readme
       ( createProjectMds
       ) where

import Data.Aeson (FromJSON (..), decode, withObject, (.!=), (.:), (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, requestHeaders, responseBody,
                            responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (ok200)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

createProjectMds :: IO ()
createProjectMds = do
    manager <- newTlsManager
    request <- parseRequest "http://api.github.com/users/kowainik/repos"
    let requestAccept = request
           { requestHeaders =
             [ ("Accept", "application/vnd.github.v3+json")
             , ("User-Agent", "kowainik")
             ]
           }
    response <- httpLbs requestAccept manager

    if (responseStatus response == ok200) then do
        let ghProjects = ghProjectsWithoutThis response

        for_ ghProjects $ makeReadmeMD manager

    else error "Status code is not ok"
  where
    -- | Projects without this one.
    ghProjectsWithoutThis :: Response ByteString -> [GitHubProject]
    ghProjectsWithoutThis response = filter validProject
        $ fromMaybe (error "Can not get the projects list")
            $ decode @[GitHubProject] $ responseBody response
      where
        -- not forks and not this project
        validProject :: GitHubProject -> Bool
        validProject GitHubProject{..} = ghpName /= "kowainik.github.io" && not ghpIsFork

    -- Formalise md files
    makeReadmeMD :: Manager -> GitHubProject -> IO ()
    makeReadmeMD manager gp@GitHubProject{..} = do
        request <- parseRequest $ "https://raw.githubusercontent.com/kowainik/"
                               ++ T.unpack ghpName
                               ++ "/master/README.md"
        response <- httpLbs request manager
        let responseText = LT.unlines $ tail $ LT.lines $ T.decodeUtf8 $ responseBody response

        let filepath :: FilePath = "projects/" ++ T.unpack ghpName ++ ".md"
        T.writeFile filepath $ createMdHeader gp <> responseText

    createMdHeader :: GitHubProject -> LT.Text
    createMdHeader GitHubProject{..} = LT.fromStrict $ T.unlines
        [ "---"
        , "title: " <> (toTitleName ghpName)
        , "link: " <> ghpName
        , "language: " <> ghpLanguage
        , "stars: " <> T.pack (show ghpStars)
        , "description: \"" <> ghpDesc <> "\""
        , "---"
        , ""
        ]

    toTitleName :: Text -> Text
    toTitleName name = "\"" <> T.unwords (T.splitOn "-" name) <> "\""


data GitHubProject = GitHubProject
    { ghpName     :: Text
    , ghpLanguage :: Text
    , ghpStars    :: Int
    , ghpIsFork   :: Bool
    , ghpDesc     :: Text
    } deriving (Show)

instance FromJSON GitHubProject where
    parseJSON = withObject "GitHubProject" $ \o -> do
        ghpName     <- o .:  "name"
        ghpLanguage <- o .:? "language" .!= "Docs"
        ghpStars    <- o .:  "stargazers_count"
        ghpIsFork   <- o .:  "fork"
        ghpDesc     <- o .:  "description"
        pure GitHubProject{..}
