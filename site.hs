import Data.List (nub)
import Data.Monoid ((<>))
import Hakyll (Context, Identifier, Item (..), Pattern, Rules, applyAsTemplate, buildTags, compile,
               compressCssCompiler, constField, copyFileCompiler, create, dateField, defaultContext,
               field, fromCapture, getTags, hakyll, idRoute, listField, loadAll,
               loadAndApplyTemplate, makeItem, match, pandocCompiler, recentFirst, relativizeUrls,
               route, setExtension, tagsRules, templateBodyCompiler, (.||.))

import Kowainik.Project (makeProjectContext)
import Kowainik.Social (makeSocialContext)
import Kowainik.Team (makeTeamContext)

main :: IO ()
main = hakyll $ do
    match ("images/**" .||. "fonts/**" .||. "js/*"  .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Main page
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let ctx = makeProjectContext <> makeSocialContext <> makeTeamContext <> defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/project.html" ctx
                >>= loadAndApplyTemplate "templates/team.html" ctx
                >>= loadAndApplyTemplate "templates/main.html" ctx
                >>= relativizeUrls

    -- build up tags
    -- Posts pages
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= \i -> getTags (itemIdentifier i)
            >>= \tgs -> loadAndApplyTemplate "templates/post.html" (postCtxWithTags tgs) i
            >>= loadAndApplyTemplate "templates/posts-default.html" (postCtxWithTags tgs)
            >>= relativizeUrls

        -- All posts page
    create ["posts.html"] $ compilePosts "Posts" "templates/posts.html" "posts/*"

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        compilePosts title "templates/tag.html" pattern

    -- Render the 404 page, we don't relativize URL's here.
    create ["404.html"] $ do
        route idRoute
        compile $ do
            let ctx = defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/404.html" ctx

    match "templates/*" $ compile templateBodyCompiler


compilePosts :: String -> Identifier -> Pattern -> Rules ()
compilePosts title page pat = do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let ids = map itemIdentifier posts
            tagsList <- nub . concat <$> traverse getTags ids
            let ctx =  listField "tagsList2"
                                       (field "tag2" $ pure . itemBody)
                                       (traverse makeItem tagsList)
                   <> constField "title" title
                   <> listField "posts" postCtx (return posts)
                   <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate page ctx
                >>= loadAndApplyTemplate "templates/posts-default.html" ctx
                >>= relativizeUrls


-- Context to used for posts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags = listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
