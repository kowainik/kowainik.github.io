module Kowainik.Nav
    ( Nav (..)
    , makeNavContext
    , mainNav
    , bookNav
    ) where

import Data.Char (toLower)
import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

import qualified Data.List as Str (words)


data Nav = Nav
    { nName :: !String
    , nId   :: !String
    }

mkNav :: String -> Nav
mkNav str = Nav
    { nName = str
    , nId = intercalate "-" $ map toLowerStr $ Str.words str
    }
  where
    toLowerStr :: String -> String
    toLowerStr []     = []
    toLowerStr (x:xs) = toLower x : xs

navName, navId :: Context Nav
navName = field "navName" $ pure . nName . itemBody
navId   = field "navId"   $ pure . nId   . itemBody

mainNav :: Compiler [Item Nav]
mainNav = traverse makeItem
    [ mkNav "Projects"
    , mkNav "About"
    , mkNav "Team"
    , mkNav "Blog"
    , mkNav "Contact us"
    ]

bookNav :: Compiler [Item Nav]
bookNav = traverse makeItem
    [ mkNav "About book"
    , mkNav "Audience"
    , mkNav "Goals"
    , mkNav "Table of Contents"
    , mkNav "Feedback"
    , mkNav "Authors"
    ]

makeNavContext :: Compiler [Item Nav] -> Context a
makeNavContext = listField "nav" (navName <> navId)
