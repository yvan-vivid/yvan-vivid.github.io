--------------------------------------------------------------------------------
-- Yvan Vivid -- Date 13 : 1 ++ 0 ++ 2 ++ 6 -- (2018)
--
-- This is my Hakyll blog.
-- There are not many like it and this one is mine.
-- Without me, my blog is useless.
-- Without my blog, I am perfectly capable of doing other things, albeit, at the
-- loss of the satisfaction I might find in this activity, and perhaps the
-- gain of some alterative source of creative expressiveness.
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc.Options

import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- adding latex support for pandoc compiler from
--    http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
-- with modifications to account for Extensions being some kind of bitmask
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    writerOptions = defaultHakyllWriterOptions {
      writerExtensions = defaultExtensions `mappend` mathExtensions,
      writerHTMLMathMethod = MathJax ""
    }
    mathExtensions = extensionsFromList
      [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
