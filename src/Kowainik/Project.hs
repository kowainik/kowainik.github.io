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
    [ Project "Project" "cabin"     "Project is currently in progress" "1"
    , Project "Project" "cake"      "Project is currently in progress" "2"
    , Project "Project" "circus"    "Project is currently in progress" "3"
    , Project "Project" "game"      "Project is currently in progress" "4"
    , Project "Project" "safe"      "Project is currently in progress" "5"
    , Project "Project" "submarine" "Project is currently in progress" "6"
    ]

makeProjectContext :: Context a
makeProjectContext = listField "projects" (projectName <> projectImage <> projectDesc <> projectId) allProjects
