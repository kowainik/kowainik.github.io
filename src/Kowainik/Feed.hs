module Kowainik.Feed
       ( feedCompiler
       ) where


import Hakyll (Compiler, Context, Identifier, Item, Rules, bodyField, compile, create, dateField,
               defaultContext, idRoute, loadAllSnapshots, recentFirst, route)
import Hakyll.Web.Feed (FeedConfiguration (..))


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Kowainik Blog"
    , feedDescription = "We are writing about what we find fascinating or \
                        \exciting. Our blog's aim is to share our knowledge about functional \
                        \programming, distribute best-practices based on our extensive experience and \
                        \tell about various topics from beginner-friendly introductions to advanced \
                        \type-level tricks."
    , feedAuthorName  = "Kowainik"
    , feedAuthorEmail = "xrom.xkov@gmail.com"
    , feedRoot        = "https://kowainik.github.io"
    }

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

-- | Context to used for feed posts
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

feedContext :: Context String
feedContext = postCtx <> bodyField "description"

feedCompiler :: Identifier -> FeedRenderer -> Rules ()
feedCompiler feedName renderer = create [feedName] $ do
    route idRoute
    compile $
        loadAllSnapshots "posts/*" "content"
        >>= fmap (take 10) . recentFirst
        >>= renderer feedConfiguration feedContext
