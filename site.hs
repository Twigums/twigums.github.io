--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import              Data.Monoid (mappend)
import              Data.Maybe (fromMaybe)
import              Control.Monad (forM_)
import qualified    Data.Text as T
import              System.FilePath ((</>))

import              Hakyll
import              Slug (toSlug)
import              Compilers (sassCompiler, tsCompiler)

--------------------------------------------------------------------------------
root :: String
root = "https://twigums.github.io"

dirSrc :: FilePath
dirSrc = "src"

dirTemplates, dirBlogs, dirTab :: FilePath
dirTemplates = dirSrc </> "templates"
dirBlogs = dirSrc </> "blogs"
dirTab = dirSrc </> "tabs"

templateDefault, templateBlog, templateSitemap :: Identifier
templateDefault = fromFilePath $ dirTemplates </> "default.html"
templateBlog = fromFilePath $ dirTemplates </> "blog_post.html"
templateSitemap = fromFilePath $ dirTemplates </> "sitemap.xml"

tabHome, tabTech, tabArt, tabBlog, tabAbout :: Pattern
tabHome = fromGlob $ dirTab </> "home.md"
tabTech = fromGlob $ dirTab </> "tech.md"
tabArt = fromGlob $ dirTab </> "art.md"
tabBlog = fromGlob $ dirTab </> "blog.md"
tabAbout = fromGlob $ dirTab </> "about.md"

blogs :: Pattern
blogs = fromGlob $ dirBlogs </> "*"

routeTab :: Routes
routeTab =
    gsubRoute (dirTab <> "/") (const "") `composeRoutes`
    gsubRoute "\\.md$" (const "/index.html")

main :: IO ()
main = hakyllWith config $ do
    match (fromGlob $ dirTemplates </> "*") $ compile templateBodyCompiler

    forM_ [
        "images/*",
        "src/robots.txt"
        ] $ \f -> match f $ do
        route   $ gsubRoute "src/" (const "")
        compile copyFileCompiler

    scssPartialDep <- makePatternDependency "src/scss/_*.scss"
    match "src/scss/_*.scss" $ compile getResourceBody
    rulesExtraDependencies [scssPartialDep] $
        match "src/scss/default.scss" $ do
            route   $ constRoute "css/default.css"
            compile sassCompiler

    tsPartialDep <- makePatternDependency "src/ts/*.ts"
    rulesExtraDependencies [tsPartialDep] $
        match "src/ts/main.ts" $ do
            route   $ constRoute "js/main.js"
            compile tsCompiler

    match tabHome $ do
        route   $ constRoute "index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate templateDefault ctxSite
            >>= relativizeUrls

    match tabBlog $ do
        route   $ routeTab
        compile $ do
            posts <- recentFirst =<< loadAll blogs
            let ctxIndex =
                    listField "blogs" ctxPosts (return posts) <>
                    ctxSite

            pandocCompiler
                >>= applyAsTemplate ctxIndex
                >>= loadAndApplyTemplate templateDefault ctxIndex
                >>= relativizeUrls

    match (
        tabTech
        .||. tabArt
        .||. tabAbout
        ) $ do
        route   $ routeTab
        compile $ pandocCompiler
            >>= loadAndApplyTemplate templateDefault ctxSite
            >>= relativizeUrls

    match blogs $ do
        let ctx = constField "type" "article" <> ctxPosts

        route   $ metadataRoute (titleRoute "blogs/")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate templateBlog ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate templateDefault ctx
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll blogs
            singlePages <- loadAll (
                tabHome
                .||. tabTech
                .||. tabArt
                .||. tabBlog
                .||. tabAbout
                )
            let pages = posts <> singlePages
                ctxSitemap =
                    constField "root" root <>
                    listField "pages" ctxPosts (return pages) <>
                    ctxSite
            makeItem ""
                >>= loadAndApplyTemplate templateSitemap ctxSitemap

config :: Configuration
config = defaultConfiguration
    {
        destinationDirectory = "docs"
    }

--------------------------------------------------------------------------------

titleRoute :: FilePath -> Metadata -> Routes
titleRoute parent = constRoute . (fileNameFromTitle parent)

-- turn title into Text, slugify, then convert it back into a string with ".html"
fileNameFromTitle :: FilePath -> Metadata -> FilePath
fileNameFromTitle parent = (parent ++) . T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta

-- gets the title using lookupString from Metadata
-- returns either the title or "no title" as a string
getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"

ctxSite :: Context String
ctxSite =
    constField "path" "" <>
    defaultContext

ctxPosts :: Context String
ctxPosts =
    constField "root" root <>
    dateField "date" "%B %e, %Y" <>
    ctxSite
