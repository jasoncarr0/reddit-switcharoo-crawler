{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Main where

import Reddit
import Reddit.Types.Listing (ListingType (..))
import Reddit.Types.Subreddit (SubredditName)
import Reddit.Types.Post
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Text
import SwitchCrawler.Parse

switcharooName :: SubredditName
switcharooName = R "switcharoo"

switcharooOptions :: RedditOptions
switcharooOptions = defaultRedditOptions { customUserAgent = Just "haskell:temp.switcharoo_scraper:v0.1.0" }

main :: IO ()
main = do
    return ()

runSwitcharoo :: MonadIO m => RedditT m a -> m (Either (APIError RedditError) a)
runSwitcharoo = runRedditWith switcharooOptions

postToLinkedComment :: Post -> Maybe (PostID, CommentID, Int)
postToLinkedComment p = do
    link <- postToLink p
    toMaybe $ doParsePermalink link where
        toMaybe (Right val) = Just val
        toMaybe (Left e) = Nothing

postToLink :: Post -> Maybe Text
postToLink (Post {content = Link text}) = Just text
postToLink _ = Nothing

switcharooPosts :: MonadIO m => RedditT m [Post]
switcharooPosts = subredditAll New (Just switcharooName)

subredditAll :: MonadIO m => ListingType -> Maybe SubredditName -> RedditT m [Post]
subredditAll lt sub = Prelude.concat <$> subredditAll' Nothing where
    subredditAll' :: MonadIO m => Maybe (PaginationOption PostID) -> RedditT m [[Post]]
    subredditAll' pag = do
        Listing before' after' posts <- getPosts' (Options pag Nothing) lt sub
        case after' of  Just b -> (posts :) <$> (subredditAll' (Just (After b)))
                        Nothing -> return [posts]

    

