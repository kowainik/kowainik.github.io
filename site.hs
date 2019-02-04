{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

import Hakyll (Context, Identifier, Item (..), MonadMetadata, Pattern, Rules, applyAsTemplate,
               buildTags, compile, compressCssCompiler, constField, copyFileCompiler, create,
               dateField, defaultContext, defaultHakyllReaderOptions, defaultHakyllWriterOptions,
               field, fromCapture, functionField, getMetadata, getResourceString, getTags, hakyll,
               idRoute, listField, loadAll, loadAndApplyTemplate, lookupString, makeItem, match,
               pandocCompiler, recentFirst, relativizeUrls, renderPandocWith, route, setExtension,
               tagsRules, templateBodyCompiler, (.||.))
import Text.Pandoc.Options (WriterOptions (..))

import Kowainik.Project (makeProjectContext)
import Kowainik.Readme (createProjectMds)
import Kowainik.Social (makeSocialContext)
import Kowainik.Team (TeamMember, makeTeamContext, parseTeam)

import qualified Data.Text as T


main :: IO ()
main = createProjectMds
    >> parseTeam "team.json"
    >>= mainHakyll

mainHakyll :: [TeamMember] -> IO ()
mainHakyll team = hakyll $ do
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
            let ctx = makeProjectContext <> makeSocialContext <> makeTeamContext team <> defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/team.html" ctx
                >>= loadAndApplyTemplate "templates/main.html" ctx
                >>= relativizeUrls

    -- Posts pages
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            i   <- getResourceString
            pandoc <- renderPandocWith defaultHakyllReaderOptions withToc i
            let toc = itemBody pandoc
            tgs <- getTags (itemIdentifier i)
            let postTagsCtx = postCtxWithTags tgs <> constField "toc" toc
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postTagsCtx
                >>= loadAndApplyTemplate "templates/posts-default.html" postTagsCtx
                >>= relativizeUrls

    -- All posts page
    create ["posts.html"] $ compilePosts "Posts" "templates/posts.html" "posts/*"

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag ptrn -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        compilePosts title "templates/tag.html" ptrn


    ----------------------------------------------------------------------------
    -- Project pages
    ----------------------------------------------------------------------------
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/readme.html" defaultContext
            >>= loadAndApplyTemplate "templates/posts-default.html" defaultContext
            >>= relativizeUrls

    -- All projects page
    create ["projects.html"] $ compileProjects "Projects" "templates/readmes.html" "projects/*"


    -- Render the 404 page, we don't relativize URL's here.
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem ""
            >>= applyAsTemplate defaultContext
            >>= loadAndApplyTemplate "templates/404.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

-- | Compose TOC from the markdown.
withToc :: WriterOptions
withToc = defaultHakyllWriterOptions
    { writerTableOfContents = True
    , writerTOCDepth = 4
    , writerTemplate = Just "$toc$"
    }

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
        let projectsCtx = stripExtension <> defaultContext
        let ctx = constField "title" title
               <> listField "readmes" projectsCtx (pure projects)
               <> projectsCtx

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls
  where
    moreStarsFirst :: MonadMetadata m => [Item a] -> m [Item a]
    moreStarsFirst = sortByM $ getItemStars . itemIdentifier
      where
        sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
        sortByM f xs = map fst . sortBy (flip $ comparing snd) <$>
            mapM (\x -> (x,) <$> f x) xs

    getItemStars
        :: MonadMetadata m
        => Identifier    -- ^ Input page
        -> m Int         -- ^ Parsed GitHub Stars
    getItemStars id' = do
        metadata <- getMetadata id'
        let mbStar = lookupString "stars" metadata >>= readMaybe @Int

        maybe starError pure mbStar
      where
        starError = error "Couldn't parse stars"


-- | Removes the @.html@ suffix in the post URLs.
stripExtension :: Context a
stripExtension = functionField "stripExtension" $ \args _ -> case args of
    [k] -> pure $ maybe k toString (T.stripSuffix ".html" $ toText k)
    _   -> error "relativizeUrl only needs a single argument"

-- Context to used for posts
postCtx :: Context String
postCtx = stripExtension
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags =
    listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
