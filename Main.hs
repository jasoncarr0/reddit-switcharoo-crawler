{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Reddit
import Reddit.Types.Listing (ListingType (..))
import Reddit.Types.Subreddit (SubredditName)
import Reddit.Types.Comment
import Reddit.Types.Post
import Control.Monad.IO.Class
import Control.Monad.State.Class
import qualified Data.Text as T
import SwitchCrawler.Parse
import SwitchCrawler.GraphBuilder

switcharooName :: SubredditName
switcharooName = R "switcharoo"

switcharooOptions :: RedditOptions
switcharooOptions = defaultRedditOptions { customUserAgent = Just "haskell:temp.switcharoo_scraper:v0.1.0" }

main :: IO ()
main = do
    return ()

runSwitcharoo :: MonadIO m => RedditT m a -> m (Either (APIError RedditError) a)
runSwitcharoo = runRedditWith switcharooOptions


commentIdsToLinked :: MonadIO m => [CommentID] -> RedditT m [Maybe CommentID]
commentIdsToLinked cs = do
    Listing _ _ cs' <- getCommentsInfo cs
    return $ doScrape <$> content <$> cs' where
        doScrape text = case doScrapePermalinks text of
            Right [(_, _, c, _)] -> Just c
            _                    -> Nothing
        content (Comment {body=c}) = c

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

    

