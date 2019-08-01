--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import Data.Foldable (toList)
import           Hakyll
import qualified GitHub as GH


--------------------------------------------------------------------------------
main :: IO ()
main = do
    repos <- listRepos

    hakyll $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

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
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["posts.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

        create ["projects.html"] $ do
            route idRoute
            compile $ do
                let projectCtx = listField "projects" mempty (fmap sequence $ makeItem repos)

                makeItem ""
                    >>= loadAndApplyTemplate "templates/projects.html" projectCtx
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
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    teaserField "teaser" "content" `mappend`
    defaultContext

repoCtx :: GH.Repo -> Context String
repoCtx repo =
    constField "url" (show . GH.repoUrl $ repo) `mappend`
    constField "name" (show . GH.repoName $ repo) `mappend`
    defaultContext

listRepos :: IO [GH.Repo]
listRepos = do
    let name = GH.mkName Nothing "jhuizy"
    reposOrError <- GH.executeRequest' $ GH.userReposR name GH.RepoPublicityPublic GH.FetchAll
    return $ either (const []) toList reposOrError