module Kowainik.Team
       ( TeamM (..)
       , makeTeamContext
       ) where

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
    [ TeamM "Dmitrii Kovanikov"
            "chshersh"
            "Haskell Adept"
            "dmitrii.kovanikov"
            "Haskell Adept, Functional Programming enthusiast, anime lover. I like to discover elegant solutions for sophisticated software problems by squeezing maximum from the Haskell programming language. I enjoy discussions about programming languages design. Before founding Kowainik organization, I had experience in cryptocurrency and full-stack web-applications development with Haskell. Also, I was <a href='https://github.com/jagajaga/FP-Course-ITMO' target=_blank >teaching Haskell</a> to the students at the ITMO University."
            "1"
    , TeamM "Veronika Romashkina"
            "vrom911"
            "❤️: Haskell, 🍫, 🍷"
            "veronika.romashkina"
            "I'm a functional programming enthusiast who puts effort into the growth of the Haskell open source community. I'm truly passionate about Haskell. I enjoy exploring new topics and never stop learning and improving my skills."
            "2"
    , TeamM "Vladislav Sabanov"
            "willbasky"
            "Haskell Developer"
            "vladislav.sabanov"
            "I am happy to program in Haskell, devoting much time to it. I do not leave issues until they are solved. I continuously learn and dig into the heart of things. Even my enthusiasm for generative art is associated with the coding."
            "3"
    ]

makeTeamContext :: Context a
makeTeamContext = listField "team" ( memberName  <> memberNick
                                  <> memberImage <> memberAbout
                                  <> memberId    <> memberWho )
                                   allTeam
