module Kowainik.Team
       ( TeamM (..)
       , makeTeamContext
       ) where

import Data.Monoid ((<>))
import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

data TeamM = TeamM
    { tmName  :: String
    , tmNick  :: String
    , tmWho   :: String
    , tmImage :: String
    , tmAbout :: String
    , tmId    :: String
    }

memberName, memberNick, memberWho, memberImage, memberAbout, memberId :: Context TeamM
memberName  = field "memberName"  $ pure . tmName  . itemBody
memberNick  = field "memberNick"  $ pure . tmNick  . itemBody
memberWho   = field "memberWho"   $ pure . tmWho   . itemBody
memberImage = field "memberImage" $ pure . tmImage . itemBody
memberAbout = field "memberAbout" $ pure . tmAbout . itemBody
memberId    = field "memberId"    $ pure . tmId    . itemBody

allTeam :: Compiler [Item TeamM]
allTeam = traverse makeItem
    [ TeamM "Dmitry Kovanikov"
            "chshersh"
            "Haskell Developer" "dmitry.kovanikov.jpg"
            "Functional programmer, anime lover, FP enthusiast, Idris fan and somehow is a Haskell & Compilers lecturer at the ITMO"
            "1"
    , TeamM "Veronika Romashkina"
            "vrom911"
            "Haskell Developer"
            "veronika.romashkina.jpeg"
            "❤️: Haskell, 🍫, 🍷"
            "2"
    ]

makeTeamContext :: Context a
makeTeamContext = listField "team" ( memberName  <> memberNick
                                  <> memberImage <> memberAbout
                                  <> memberId    <> memberWho )
                                   allTeam
