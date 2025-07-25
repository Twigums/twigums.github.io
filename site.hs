--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import              Data.Monoid (mappend)
import              Data.Maybe (fromMaybe)
import              Control.Monad (forM_)
import qualified    Data.Text as T
import              System.FilePath ((</>))

import              Hakyll
import              Slug (toSlug)

--------------------------------------------------------------------------------
root :: String
root = "https://twigums.github.io"

path_to_template :: FilePath
path_to_template = "src/templates/"

path_to_blogs :: FilePath
path_to_blogs = "src/blogs"

path_to_tabs :: [FilePath]
path_to_tabs = [
    "src/tabs/home.md",
    "src/tabs/tech.md",
    "src/tabs/art.md",
    "src/tabs/blog.md",
    "src/tabs/contact.md"
    ]

makePattern :: FilePath -> FilePath -> Pattern
makePattern path1 path2 = fromGlob (path1 </> path2)

makeIdentifier :: FilePath -> FilePath -> Identifier
makeIdentifier path1 path2 = fromFilePath (path1 </> path2)

main :: IO ()
main = hakyllWith config $ do
    match (makePattern path_to_template "*") $ compile templateBodyCompiler

    forM_ [
        "images/*",
        "src/css/*",
        "src/js/*"
        ] $ \f -> match f $ do
        route   $ gsubRoute "src/" (const "")
        compile copyFileCompiler

    match "src/tabs/home.md" $ do
        route   $ constRoute "index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate (makeIdentifier path_to_template "default.html")   defaultContext
            >>= relativizeUrls

    match "src/tabs/blog.md" $ do
        route   $ gsubRoute "src/tabs/" (const "") `composeRoutes`
                  gsubRoute "\\.md$" (const "/index.html")

        compile $ do
            blogs <- recentFirst =<< loadAll (makePattern path_to_blogs "*")
            let indexCtx =
                    listField "blogs" postCtx (return blogs) <>
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate (makeIdentifier path_to_template "default.html")  indexCtx
                >>= relativizeUrls

    match (fromList [
        "src/tabs/tech.md",
        "src/tabs/art.md",
        "src/tabs/contact.md"
        ]) $ do
        route   $ gsubRoute "src/tabs/" (const "") `composeRoutes` 
                  gsubRoute "\\.md$" (const "/index.html")

        compile $ pandocCompiler
            >>= loadAndApplyTemplate (makeIdentifier path_to_template "default.html") defaultContext
            >>= relativizeUrls

    match (makePattern path_to_blogs "*") $ do
        let ctx = constField "type" "article" <> postCtx

        route   $ metadataRoute (titleRoute "blogs/")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate (makeIdentifier path_to_template "blog_post.html")     ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate (makeIdentifier path_to_template "default.html")  ctx
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            blogs <- recentFirst =<< loadAll (makePattern path_to_blogs "*")
            singlePages <- loadAll (fromList $ map (makeIdentifier "") path_to_tabs)
            let pages = blogs <> singlePages
                sitemapCtx =
                    constField "root" root <>
                    listField "pages" postCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate (makeIdentifier path_to_template "sitemap.xml")   sitemapCtx

config :: Configuration
config = defaultConfiguration
    {
        destinationDirectory = "docs"
    }

titleRoute :: FilePath -> Metadata -> Routes
titleRoute parent = constRoute . (fileNameFromTitle parent)

-- turn title into Text, slugify, then convert it back into a string with ".html"
fileNameFromTitle :: FilePath -> Metadata -> FilePath
fileNameFromTitle parent = (parent ++) . T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta

-- gets the title using lookupString from Metadata
-- returns either the title or "no title" as a string
getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    constField "root" root <>
    dateField "date" "%B %e, %Y" <>
    defaultContext
