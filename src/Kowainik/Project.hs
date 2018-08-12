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
    [ Project "Summoner" "summoner" "Tool for creating completely configured production Haskell projects" "1"
    , Project "Smuggler" "smuggler" "Smuggle all imports" "2"
    , Project "Relude" "relude" "Reload your Prelude" "3"
    , Project "Containers Backpack" "containers-backpack" "Backpack interface for containers" "4"
    , Project "Idris Patricia" "idris-patricia" "Idris implementation of patricia tree" "5"
    , Project "Typerep-map" "typerep-map" "Efficient implementation of Map with types as keys" "6"
    ]

makeProjectContext :: Context a
makeProjectContext = listField "projects" (projectName <> projectImage <> projectDesc <> projectId) allProjects
