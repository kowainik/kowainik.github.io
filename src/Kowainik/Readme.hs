module Kowainik.Readme
       ( createProjectMds
       ) where

import Data.Aeson (FromJSON (..), eitherDecode', withObject, (.!=), (.:), (.:?))
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, requestHeaders, responseBody,
                            responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (ok200)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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

    if responseStatus response == ok200 then do
        let ghProjects = ghProjectsWithoutThis response

        createDirectoryIfMissing True "projects"
        for_ ghProjects $ makeReadmeMD manager

    else error "Status code is not ok"
  where
    -- | Projects without this one.
    ghProjectsWithoutThis :: Response LByteString -> [GitHubProject]
    ghProjectsWithoutThis response = filter validProject
        $ either (error . toText) id
        $ eitherDecode' @[GitHubProject] $ responseBody response
      where
        -- not forks and not this project
        validProject :: GitHubProject -> Bool
        validProject GitHubProject{..} = ghpName /= "kowainik.github.io" && not ghpIsFork

    -- Formalise md files
    makeReadmeMD :: Manager -> GitHubProject -> IO ()
    makeReadmeMD manager gp@GitHubProject{..} = do
        request <- parseRequest $ "https://raw.githubusercontent.com/kowainik/"
                               ++ toString ghpName
                               ++ "/master/README.md"
        response <- httpLbs request manager
        let responseText = LT.unlines $ drop 1 $ LT.lines $ T.decodeUtf8 $ responseBody response

        let filepath :: FilePath = "projects" </> toString ghpName ++ ".md"
        T.writeFile filepath $ createMdHeader gp <> responseText

    createMdHeader :: GitHubProject -> LText
    createMdHeader GitHubProject{..} = toLazy $ unlines
        [ "---"
        , "title: " <> toTitleName ghpName
        , "link: " <> ghpName
        , "language: " <> ghpLanguage
        , "stars: " <> show ghpStars
        , "description: \"" <> ghpDesc <> "\""
        , "---"
        , ""
        ]

    toTitleName :: Text -> Text
    toTitleName name = "\"" <> unwords (T.splitOn "-" name) <> "\""


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
