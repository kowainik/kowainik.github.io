module Kowainik.Social
       ( Social (..)
       , makeSocialContext
       ) where

import Data.Monoid ((<>))
import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

data Social = Social
    { sName :: String
    , sLink :: String
    }

socialName, socialLink :: Context Social
socialName = field "socialName" $ pure . sName . itemBody
socialLink = field "socialLink" $ pure . sLink . itemBody

allSocials :: Compiler [Item Social]
allSocials = traverse makeItem
    [ Social "twitter"  "https://twitter.com/kowainik"
    , Social "github"   "https://github.com/kowainik"
    , Social "reddit"   "https://www.reddit.com/user/kowainik"
    , Social "linkedin" "https://www.linkedin.com/company/kowainik"
    , Social "telegram" "https://t.me/kowainik"
    ]

makeSocialContext :: Context a
makeSocialContext = listField "socials" (socialName <> socialLink) allSocials
