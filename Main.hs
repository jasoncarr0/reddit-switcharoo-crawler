{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Reddit
import Reddit.Types.Listing (ListingType (..))
import Reddit.Types.Subreddit (SubredditName)
import Reddit.Types.Comment
import Reddit.Types.Post
import Control.Exception (assert)
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import SwitchCrawler.Parse
import SwitchCrawler.GraphBuilder

switcharooName :: SubredditName
switcharooName = R "switcharoo"

switcharooOptions :: RedditOptions
switcharooOptions = defaultRedditOptions 
    { customUserAgent = Just "haskell:temp.switcharoo_scraper:v0.1.0" }

main :: IO ()
main = do
    doCrawl
    return ()

doCrawl :: IO (M.Map CommentID [CommentID])
doCrawl = do
    --assume we succeed
    Right cg <- runSwitcharooBuilder $ do
        posts <- switcharooPosts
        let comments = catMaybes $ postToLinkedComment <$> posts
        buildCommentGraph comments
    return cg


runSwitcharoo :: MonadIO m => RedditT m a -> m (Either (APIError RedditError) a)
runSwitcharoo = runRedditWith switcharooOptions

runSwitcharooBuilder :: RedditT (StateT (GraphBuilder a) IO) b -> IO (Either (APIError RedditError) b)
runSwitcharooBuilder = flip evalStateT emptyGraphBuilder . runSwitcharoo



buildCommentGraph :: (MonadIO m, MonadState (GraphBuilder CommentID) m) => [CommentID] -> RedditT m (M.Map CommentID [CommentID])
buildCommentGraph ids = do
    lift (enqueue ids) 
    buildCommentGraph'
    GraphBuilder _ map <- lift get
    return map where
        buildCommentGraph' :: (MonadIO m, MonadState (GraphBuilder CommentID) m) => RedditT m ()
        buildCommentGraph' = do
            lift $ modifyQueue (filter (/= (CommentID "")) . nub)
            nextIds <- lift $ dequeue 100
            if (not $ null nextIds) then do
                values <- commentIdsToLinked nextIds
                return $ assert (length nextIds == length values) ()
                let pairs = nextIds `zip` values
                contains <- lift $ mapContains
                lift $ enqueue $ join values
                lift $ modifyQueue (filter (not . contains))
                
                lift $ sequence (uncurry insertMap <$> pairs)
                buildCommentGraph'
            else return ()



commentIdsToLinked :: MonadIO m => [CommentID] -> RedditT m [[CommentID]]
commentIdsToLinked cs = do
    Listing _ _ cs' <- getCommentsInfo cs
    let newVal = do
         c <- cs'
         return (c, doScrape $ content c)
    --for each element in the list, if it's empty, check one level of children
    let twisted = (\(c, ls) -> if (null ls) then tryRec c else return ls) <$> newVal
    sequence twisted
    where
        doScrape text = case doScrapePermalinks text of
            Right ls -> third4 <$> ls
            _        -> []
        tryRec c = do
            r1 <- tryRec1 c 
            if (not $ null r1) then return r1 else tryRec2 c
        tryRec1 c = do
            cs2 <- getMoreChildren (parentLink c) [commentID c]
            return $ join (doScrape <$> content <$> catMaybes (actual <$> cs2))
        tryRec2 c = do
            let parID = inReplyTo c
            case parID of Nothing -> return []
                          Just p -> (doScrape . content) <$> getCommentInfo p
        actual (Actual c) = Just c
        actual _ = Nothing
        content (Comment {body=c}) = c
        third4 (a, b, c, d) = c

postToLinkedComment :: Post -> Maybe CommentID
postToLinkedComment p = do
    link <- postToLink p
    toMaybe $ snd3 <$> doParsePermalink link where
        toMaybe (Right val) = Just val
        toMaybe (Left e) = Nothing
        snd3 (a, b, c) = b

postToLink :: Post -> Maybe T.Text
postToLink (Post {content = Link text}) = Just text
postToLink _ = Nothing

switcharooPosts :: MonadIO m => RedditT m [Post]
switcharooPosts = subredditAll New (Just switcharooName)

subredditAll :: MonadIO m => ListingType -> Maybe SubredditName -> RedditT m [Post]
subredditAll lt sub = concat <$> subredditAll' Nothing where
    subredditAll' :: MonadIO m => Maybe (PaginationOption PostID) -> RedditT m [[Post]]
    subredditAll' pag = do
        Listing before' after' posts <- getPosts' (Options pag Nothing) lt sub
        case after' of  Just b -> (posts :) <$> (subredditAll' (Just (After b)))
                        Nothing -> return [posts]

    

