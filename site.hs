{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

import Hakyll (Context, Identifier, Item (..), MonadMetadata, Pattern, Rules, applyAsTemplate,
               buildTags, compile, compressCssCompiler, constField, copyFileCompiler, create,
               dateField, defaultContext, field, fromCapture, getMetadata, getTags, hakyll, idRoute,
               listField, loadAll, loadAndApplyTemplate, lookupString, makeItem, match,
               pandocCompiler, recentFirst, relativizeUrls, route, setExtension, tagsRules,
               templateBodyCompiler, (.||.))

import Kowainik.Project (makeProjectContext)
import Kowainik.Readme (createProjectMds)
import Kowainik.Social (makeSocialContext)
import Kowainik.Team (makeTeamContext)

main :: IO ()
main = createProjectMds >> mainHakyll

mainHakyll :: IO ()
mainHakyll = hakyll $ do
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
                >>= loadAndApplyTemplate "templates/team.html" ctx
                >>= loadAndApplyTemplate "templates/main.html" ctx
                >>= relativizeUrls

    -- Posts pages
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            i   <- pandocCompiler
            tgs <- getTags (itemIdentifier i)
            let postTagsCtx = postCtxWithTags tgs
            loadAndApplyTemplate "templates/post.html" postTagsCtx i
                >>= loadAndApplyTemplate "templates/posts-default.html" postTagsCtx
                >>= relativizeUrls

    -- All posts page
    create ["posts.html"] $ compilePosts "Posts" "templates/posts.html" "posts/*"

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        compilePosts title "templates/tag.html" pattern


    ----------------------------------------------------------------------------
    -- Project pages
    ----------------------------------------------------------------------------
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/readme.html" defaultContext
                >>= loadAndApplyTemplate "templates/posts-default.html" defaultContext
                >>= relativizeUrls

    -- All projects page
    create ["projects.html"] $ compileProjects "Projects" "templates/readmes.html" "projects/*"


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
        tagsList <- ordNub . concat <$> traverse getTags ids
        let ctx = postCtxWithTags tagsList
               <> constField "title" title
               <> listField "posts" postCtx (return posts)
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls

compileProjects :: String -> Identifier -> Pattern -> Rules ()
compileProjects title page pat = do
    route idRoute
    compile $ do
        projects <- moreStarsFirst =<< loadAll pat
        let ctx = constField "title" title
               <> listField "readmes" defaultContext (pure projects)
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls
  where
    moreStarsFirst :: MonadMetadata m => [Item a] -> m [Item a]
    moreStarsFirst = sortByM $ getItemStars . itemIdentifier
      where
        sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
        sortByM f xs = fmap (map fst . sortBy (flip $ comparing snd)) $
                       mapM (\x -> (x,) <$> (f x)) xs

    getItemStars :: MonadMetadata m
                 => Identifier    -- ^ Input page
                 -> m Int         -- ^ Parsed GitHub Stars
    getItemStars id' = do
        metadata <- getMetadata id'
        let mbStar = lookupString "stars" metadata >>= readMaybe @Int

        maybe starError pure mbStar
      where
        starError = error "Couldn't parse stars"

-- Context to used for posts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags = listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
