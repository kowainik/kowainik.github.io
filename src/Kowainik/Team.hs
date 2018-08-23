-- | All team members information is in the @/team.json@ file.

module Kowainik.Team
       ( TeamMember (..)
       , makeTeamContext
       , parseTeam
       ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.Char (toLower)
import Hakyll (Context, field, itemBody, listField, makeItem)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.String as String (words)

-- | Information about team member.
data TeamMember = TeamMember
    { tmName  :: String
    , tmNick  :: String
    , tmWho   :: String
    , tmImage :: String
    , tmAbout :: String
    , tmId    :: String
    }

instance FromJSON TeamMember where
    parseJSON = withObject "TeamMember" $ \o -> do
        tmName  <- o .: "name"
        tmNick  <- o .: "nick"
        tmWho   <- o .: "who"
        tmAbout <- o .: "about"
        let tmImage = intercalate "." $ String.words $ map toLower tmName
        let tmId = tmNick
        pure TeamMember{..}

memberName, memberNick, memberWho, memberImage, memberAbout, memberId :: Context TeamMember
memberName  = field "memberName"  $ pure . tmName  . itemBody
memberNick  = field "memberNick"  $ pure . tmNick  . itemBody
memberWho   = field "memberWho"   $ pure . tmWho   . itemBody
memberImage = field "memberImage" $ pure . tmImage . itemBody
memberAbout = field "memberAbout" $ pure . tmAbout . itemBody
memberId    = field "memberId"    $ pure . tmId    . itemBody

makeTeamContext :: [TeamMember] -> Context a
makeTeamContext team = listField "team"
    ( memberName  <> memberNick
   <> memberImage <> memberAbout
   <> memberId    <> memberWho )
    $ traverse makeItem team

parseTeam :: FilePath -> IO [TeamMember]
parseTeam teamPath = do
    teamJson <- LBS.readFile teamPath
    let eitherTeam = eitherDecode teamJson
    case eitherTeam of
        Left e     -> error $ toText e
        Right team -> return team
