-- | All team members information is in the @/team.json@ file.

module Kowainik.Team
    ( TeamMember (..)
    , makeTeamContext
    , makeCreatorsContext
    , makeVolunteersContext
    , parseTeam
    ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?), (.!=))
import Data.Char (toLower)
import Hakyll (Context, field, itemBody, listField, makeItem)

import qualified Data.String as String (words)


-- | Information about team member.
data TeamMember = TeamMember
    { tmName    :: String
    , tmNick    :: String
    , tmWho     :: String
    , tmWeb     :: Maybe String
    , tmImage   :: String
    , tmAbout   :: String
    , tmSponsor :: Bool
    , tmId      :: String
    }

instance FromJSON TeamMember where
    parseJSON = withObject "TeamMember" $ \o -> do
        tmName    <- o .: "name"
        tmNick    <- o .: "nick"
        tmWho     <- o .: "who"
        tmWeb     <- o .:? "web"
        tmAbout   <- o .: "about"
        tmSponsor <- o .:? "sponsor" .!= False
        let tmImage = intercalate "." $ String.words $ map toLower tmName
        let tmId = tmNick
        pure TeamMember{..}

memberName, memberNick, memberWho, memberWeb, memberImage, memberAbout, memberSponsor, memberId :: Context TeamMember
memberName  = field "memberName"  $ pure . tmName  . itemBody
memberNick  = field "memberNick"  $ pure . tmNick  . itemBody
memberWho   = field "memberWho"   $ pure . tmWho   . itemBody
memberImage = field "memberImage" $ pure . tmImage . itemBody
memberAbout = field "memberAbout" $ pure . tmAbout . itemBody
memberId    = field "memberId"    $ pure . tmId    . itemBody
memberWeb   = field "memberWeb"   $ maybe empty pure . tmWeb . itemBody
memberSponsor = field "memberSponsor" $ (\p -> if p then pure "" else empty) . tmSponsor . itemBody

makeVolunteersContext :: [TeamMember] -> Context a
makeVolunteersContext = makeTeamContext "volunteers"

makeCreatorsContext :: [TeamMember] -> Context a
makeCreatorsContext = makeTeamContext "creators"

makeTeamContext :: String -> [TeamMember] -> Context a
makeTeamContext txt = listField txt
    ( memberName
    <> memberNick
    <> memberImage
    <> memberAbout
    <> memberId
    <> memberWho
    <> memberWeb
    <> memberSponsor
    )
    . traverse makeItem

parseTeam :: FilePath -> IO [TeamMember]
parseTeam teamPath = do
    teamJson <- readFileLBS teamPath
    let eitherTeam = eitherDecode teamJson
    case eitherTeam of
        Left e     -> error $ toText e
        Right team -> return team
