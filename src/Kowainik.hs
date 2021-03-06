{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use traverseToSnd" -}

module Kowainik
    ( runKowainik
    ) where

import Data.Time (UTCTime)
import Hakyll (Compiler, Context, Identifier, Item (..), MonadMetadata, Pattern, Rules,
               applyAsTemplate, buildTags, compile, compressCssCompiler, constField,
               copyFileCompiler, create, customRoute, dateField, defaultContext,
               defaultHakyllReaderOptions, defaultHakyllWriterOptions, field, fromCapture,
               functionField, getMetadata, getResourceString, getTags, hakyll, idRoute, listField,
               loadAll, loadAndApplyTemplate, lookupString, makeItem, match, metadataRoute,
               pandocCompilerWithTransformM, relativizeUrls, renderPandocWith, route, saveSnapshot,
               setExtension, tagsRules, templateBodyCompiler, titleField, toFilePath,
               unsafeCompiler, (.||.))
import Hakyll.ShortcutLinks (applyAllShortcuts)
import Hakyll.Web.Feed (renderAtom, renderRss)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, replaceExtension, splitDirectories)
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Templates (compileTemplate)

import Kowainik.Download (getStanReport, syncStyleGuide)
import Kowainik.Feed (feedCompiler)
import Kowainik.Nav (bookNav, mainNav, makeNavContext)
import Kowainik.Project (makeProjectContext)
import Kowainik.Readme (createProjectMds)
import Kowainik.Social (makeSocialContext)
import Kowainik.Team (TeamMember, makeCreatorsContext, makeTeamContext, parseTeam)

import qualified Data.Text as T
import qualified Data.Time.Format as Time
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc.Walk


runKowainik :: IO ()
runKowainik = do
    noSync <- lookupEnv "NO_SYNC"

    mReport <- case noSync of
        Just _ -> pure Nothing
        Nothing -> do
            createProjectMds
            syncStyleGuide
            Just <$> getStanReport

    creators <- parseTeam "team/creators.json"

    mainHakyll creators mReport

mainHakyll :: [TeamMember] -> Maybe String -> IO ()
mainHakyll creators mReport = hakyll $ do
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
            let ctx = makeNavContext mainNav
                   <> makeProjectContext
                   <> makeSocialContext
                   <> makeCreatorsContext creators
                   <> makeTeamContext "team" creators
                   <> defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/team.html" ctx
                >>= loadAndApplyTemplate "templates/main.html" ctx
                >>= relativizeUrls

    -- books
    create ["books/do-pure-haskell-interview.html"] $ do
        route idRoute
        compile $ do
            let ctx = makeNavContext bookNav
                   <> makeSocialContext
                   <> makeCreatorsContext creators
                   <> makeTeamContext "team" creators
                   <> defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/haskell-interview-book.html" ctx
                >>= relativizeUrls

    -- Stan report page
    whenJust mReport $ \report -> create ["projects/stan/report.html"] $ do
        route idRoute
        compile (makeItem report >>= relativizeUrls)

    -- Posts pages
    match "posts/*" $ do
        route $ metadataRoute $ \metadata -> case lookupString "useShortName" metadata of
            Nothing -> setExtension "html"
            Just _  -> customRoute
                $ (`replaceExtension` "html")
                . ("posts/" ++)
                . drop 17
                . toFilePath

        compile $ do
            i   <- getResourceString
            tocWriter <- unsafeCompiler withToc
            pandoc <- renderPandocWith defaultHakyllReaderOptions tocWriter i
            let toc = itemBody pandoc
            tgs <- getTags (itemIdentifier i)
            let postTagsCtx
                   =  postCtxWithTags tgs
                   <> constField "toc" toc
                   <> makeSocialContext
            customPandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postTagsCtx
                >>= loadAndApplyTemplate "templates/posts-default.html" postTagsCtx
                >>= saveSnapshot "content"
                >>= relativizeUrls

    -- All posts page
    create ["posts.html"] $ compilePosts "Blog Posts" "templates/posts.html" "posts/*"


    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag ptrn -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        compilePosts title "templates/tag.html" ptrn

    feedCompiler "atom.xml" renderAtom
    feedCompiler "rss.xml"  renderRss

-- doesn't work ;(
--
--     -- Use short names redirects for the old posts
--     createRedirects
--         [ ("posts/build-tools.html",                "posts/2018-06-21-haskell-build-tools.md")
--         , ("posts/typerep-map.html",                "posts/2018-07-11-typerep-map-step-by-step.md")
--         , ("posts/backpack.html",                   "posts/2018-08-19-picnic-put-containers-into-a-backpack.md")
--         , ("posts/dhall-to-hlint.html",             "posts/2018-09-09-dhall-to-hlint.md")
--         , ("posts/co-log.html",                     "posts/2018-09-25-co-log.md")
--         , ("posts/hacktoberfest-2018.html",         "posts/2018-10-01-hacktoberfest.md")
--         , ("posts/hacktoberfest-2018-wrap-up.html", "posts/2018-11-01-hacktoberfest-wrap-up.md")
--         , ("posts/state-pattern-matching.html",     "posts/2018-11-18-state-pattern-matching.md")
--         , ("posts/tomland.html",                    "posts/2019-01-14-tomland.md")
--         , ("posts/style-guide.html",                "posts/2019-02-06-style-guide.md")
--         ]

    ----------------------------------------------------------------------------
    -- Project pages
    ----------------------------------------------------------------------------
    match "projects/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
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
withToc :: IO WriterOptions
withToc = compileTemplate "myToc.txt" "$toc$" >>= \case
    Left err -> error $ toText err
    Right template -> pure $ defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTOCDepth = 4
        , writerTemplate = Just template
        }

compilePosts :: String -> Identifier -> Pattern -> Rules ()
compilePosts title page pat = do
    route idRoute
    compile $ do
        posts <- recentlyUpdatedFirst =<< loadAll pat
        let ids = map itemIdentifier posts
        tagsList <- ordNub . concat <$> traverse getTags ids
        let ctx = postCtxWithTags tagsList
               <> titleField title
               <> constField "description" "Kowainik blog"
               <> listField "posts" postCtx (return posts)
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls
  where
    recentlyUpdatedFirst :: MonadMetadata m => [Item a] -> m [Item a]
    recentlyUpdatedFirst = sortByM $ getItemLastModificationTime . itemIdentifier

    getItemLastModificationTime
        :: MonadMetadata m
        => Identifier    -- ^ Input page
        -> m UTCTime     -- ^ Parsed UTC time
    getItemLastModificationTime id' = do
        metadata <- getMetadata id'
        -- lookup 'updated' from metadata
        let mUpdated = lookupString "updated" metadata >>= parseLongDate

        -- get 'created' from path as we don't have 'date' yet
        let paths = splitDirectories $ dropExtension $ toFilePath id'
        let mCreated = asumMap (parseShortDate . take shortDateLen) paths

        let errMsg = unlines
                [ "Error parsing 'updated' and 'created' times from:"
                , "  updated: " <> show (lookupString "updated" metadata)
                , "  created: " <> show paths
                ]

        maybe (error errMsg) pure (mUpdated <|> mCreated)

compileProjects :: String -> Identifier -> Pattern -> Rules ()
compileProjects title page pat = do
    route idRoute
    compile $ do
        projects <- moreStarsFirst =<< loadAll pat
        let projectsCtx = stripExtension <> defaultContext
        let ctx = titleField title
               <> constField "description" "Kowainik projects"
               <> listField "readmes" projectsCtx (pure projects)
               <> projectsCtx

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls
  where
    moreStarsFirst :: MonadMetadata m => [Item a] -> m [Item a]
    moreStarsFirst = sortByM $ getItemStars . itemIdentifier

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

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = map fst . sortBy (flip $ comparing snd) <$>
    mapM (\x -> (x,) <$> f x) xs

-- | Removes the @.html@ suffix in the post URLs.
stripExtension :: Context a
stripExtension = functionField "stripExtension" $ \args _ -> case args of
    [k] -> pure $ maybe k toString (T.stripSuffix ".html" $ toText k)
    _   -> error "relativizeUrl only needs a single argument"

{- | Our own pandoc compiler which adds anchors automatically and uses
@hakyll-shortcut-links@ library for shortcut transformations.
-}
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (applyAllShortcuts . addAnchors)

-- | Modifie a headers to add an extra anchor which links to the header.  This
-- allows you to easily copy an anchor link to a header.
addAnchors :: Pandoc.Pandoc -> Pandoc.Pandoc
addAnchors =
    Pandoc.Walk.walk addAnchor
  where
    addAnchor :: Pandoc.Block -> Pandoc.Block
    addAnchor (Pandoc.Header level attr@(id_, _, _) content) =
        Pandoc.Header level attr $ content ++
            [Pandoc.Link ("", ["anchor"], []) [Pandoc.Str "🔗"] ("#" <> id_, "")]
    addAnchor block = block

-- Context to used for posts
postCtx :: Context String
postCtx = stripExtension
    <> dateField "date" dateFormat
    <> defaultContext

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags =
    listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx

{- | Date format used in our blog posts and projects:

@
September 19, 2021
@
-}
dateFormat :: String
dateFormat = "%B %e, %Y"

{- | Parse string representation of a date using 'dateFormat'.
-}
parseLongDate :: MonadFail m => String -> m UTCTime
parseLongDate = Time.parseTimeM True Time.defaultTimeLocale dateFormat

{- | Parse string representation of a date using blog post names date format.
-}
parseShortDate :: MonadFail m => String -> m UTCTime
parseShortDate = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

shortDateLen :: Int
shortDateLen = length @[] "YYYY-MM-DD"
