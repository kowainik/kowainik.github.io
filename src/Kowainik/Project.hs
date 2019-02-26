module Kowainik.Project
       ( Project (..)
       , makeProjectContext
       ) where

import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

data Project = Project
    { pName  :: String
    , pImage :: String
    , pDesc  :: String
    , pId    :: String
    }

projectName, projectImage, projectDesc, projectId :: Context Project
projectName  = field "projectName"  $ pure . pName  . itemBody
projectImage = field "projectImage" $ pure . pImage . itemBody
projectDesc  = field "projectDesc"  $ pure . pDesc  . itemBody
projectId    = field "projectId"    $ pure . pId    . itemBody

allProjects :: Compiler [Item Project]
allProjects = traverse makeItem
    [ Project
        { pName  = "Summoner"
        , pImage = "summoner"
        , pDesc  = "Tool for creating completely configured production Haskell projects"
        , pId    = "1"
        }
    , Project
        { pName  = "Relude"
        , pImage = "relude"
        , pDesc  = "Reload your Prelude"
        , pId    = "2"
        }
    , Project
        { pName  = "Containers Backpack"
        , pImage = "containers-backpack"
        , pDesc  = "Backpack interface for containers"
        , pId    = "3"
        }
    , Project
        { pName  = "Smuggler"
        , pImage = "smuggler"
        , pDesc  = "Smuggle all imports"
        , pId    = "4"
        }
    , Project
        { pName  = "Tomland"
        , pImage = "tomland"
        , pDesc  = "Bidirectional TOML serialization"
        , pId    = "5"
        }
    , Project
        { pName  = "Typerep-map"
        , pImage = "typerep-map"
        , pDesc  = "Efficient implementation of Map with types as keys"
        , pId    = "6"
        }
    ]

makeProjectContext :: Context a
makeProjectContext = listField "projects" (projectName <> projectImage <> projectDesc <> projectId) allProjects
