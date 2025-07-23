--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe (fromMaybe)
import           Control.Monad (forM_)
import qualified Data.Text as T

import           Hakyll
import           Slug (toSlug)

--------------------------------------------------------------------------------
root :: String
root = "https://blog.com"

main :: IO ()
main = hakyllWith config $ do
    forM_ [
        "images/*",
        "css/*",
        "fonts/*"
        ] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList [
        "src/tech.md",
        "src/art.md",
        "src/blog.md",
        "src/contact.md"
        ]) $ do
        route   $ gsubRoute "src/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        let ctx = constField "type" "article" <> postCtx

        route $ metadataRoute titleRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])
            let pages = posts <> singlePages
                sitemapCtx =
                    constField "root" root <>
                    listField "pages" postCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


config :: Configuration
config = defaultConfiguration
    {
        destinationDirectory = "docs"
    }

titleRoute :: Metadata -> Routes
titleRoute = constRoute . fileNameFromTitle

-- turn title into Text, slugify, then convert it back into a string with ".html"
fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle = ("posts/" ++) . T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta

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
