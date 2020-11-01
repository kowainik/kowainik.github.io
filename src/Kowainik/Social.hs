module Kowainik.Social
    ( Social (..)
    , makeSocialContext
    ) where

import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

data Social = Social
    { sName  :: !String
    , sClass :: !String
    , sLink  :: !String
    }

socialName, socialClass, socialLink :: Context Social
socialName  = field "socialName"  $ pure . sName  . itemBody
socialClass = field "socialClass" $ pure . sClass . itemBody
socialLink  = field "socialLink"  $ pure . sLink  . itemBody

allSocials :: Compiler [Item Social]
allSocials = traverse makeItem
    [ Social "twitter"  "fab" "https://twitter.com/kowainik"
    , Social "github"   "fab" "https://github.com/kowainik"
    , Social "linkedin" "fab" "https://www.linkedin.com/company/kowainik"
    , Social "telegram" "fab" "https://t.me/kowainik"
    , Social "rss"      "fas" "/rss.xml"
    ]

makeSocialContext :: Context a
makeSocialContext = listField
    "socials"
    (socialName <> socialLink <> socialClass)
    allSocials
