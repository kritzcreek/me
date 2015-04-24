--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Data.Map (lookup)
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match "resources/images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "resources/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "resources/js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith
          defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/article.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" (topCtx `mappend` postCtx)
            >>= relativizeUrls

    match "drafts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith
          defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/article.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" (topCtx `mappend` postCtx)
            >>= relativizeUrls

    match "love/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith
          defaultHakyllReaderOptions defaultHakyllWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" (topCtx `mappend` postCtx)
            >>= relativizeUrls

    match "pages/*" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
        >>= loadAndApplyTemplate "templates/default.html" (topCtx `mappend` postCtx)
        >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "id" "archives"               `mappend`
                    (topCtx `mappend` defaultContext)

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
                    (topCtx `mappend` defaultContext)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax "" }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

loveCtx :: Context String
loveCtx =
    idField "love" `mappend`
    titleField "A Philosophy of Romantic Love" `mappend`
    defaultContext

topCtx :: Context String
topCtx = mconcat
         [ field "mathjax" mathjax ]

mathjax :: Item String -> Compiler String
mathjax item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Data.Map.lookup "math" metadata of
    Just "true" -> "<script type\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
    otherwise   -> ""
