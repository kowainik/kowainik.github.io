module Kowainik.Project
       ( Project (..)
       , makeProjectContext
       ) where

import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)


data Project = Project
    { pName  :: !String
    , pImage :: !String
    , pDesc  :: !String
    , pId    :: !String
    }

projectName, projectImage, projectDesc, projectId :: Context Project
projectName  = field "projectName"  $ pure . pName  . itemBody
projectImage = field "projectImage" $ pure . pImage . itemBody
projectDesc  = field "projectDesc"  $ pure . pDesc  . itemBody
projectId    = field "projectId"    $ pure . pId    . itemBody

allProjects :: Compiler [Item Project]
allProjects = traverse makeItem
    [ Project
        { pName  = "Stan"
        , pImage = "stan"
        , pDesc  = "Haskell Static Analyser tool"
        , pId    = "1"
        }
    , Project
        { pName  = "Summoner"
        , pImage = "summoner"
        , pDesc  = "Tool for creating completely configured production Haskell projects"
        , pId    = "2"
        }
    , Project
        { pName  = "Relude"
        , pImage = "relude"
        , pDesc  = "Reload your Prelude"
        , pId    = "3"
        }
    , Project
        { pName  = "Co-Log"
        , pImage = "co-log"
        , pDesc  = "Flexible and configurable modern Haskell logging framework"
        , pId    = "4"
        }
    , Project
        { pName  = "Tomland"
        , pImage = "tomland"
        , pDesc  = "Bidirectional TOML serialization"
        , pId    = "5"
        }
    , Project
        { pName  = "Validation Selective"
        , pImage = "validation-selective"
        , pDesc  = "Lightweight pure validation based on Applicative and Selective functors"
        , pId    = "6"
        }
    , Project
        { pName  = "Policeman"
        , pImage = "policeman"
        , pDesc  = "Haskell PVP adviser"
        , pId    = "7"
        }
    , Project
        { pName  = "Extensions"
        , pImage = "extensions"
        , pDesc  = "Parse Haskell Language Extensions"
        , pId    = "8"
        }
    , Project
        { pName  = "Colourista"
        , pImage = "colourista"
        , pDesc  = "Convenient interface for printing colourful messages"
        , pId    = "9"
        }
    ]

makeProjectContext :: Context a
makeProjectContext = listField "projects" (projectName <> projectImage <> projectDesc <> projectId) allProjects
