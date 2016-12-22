{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Main where

import Reddit
import Reddit.Types.Listing (ListingType)
import Reddit.Types.Subreddit (SubredditName)
import Control.Monad.IO.Class
import Control.Monad.State.Class

switcharooName :: SubredditName
switcharooName = R "switcharoo"

switcharooOptions :: RedditOptions
switcharooOptions = defaultRedditOptions { customUserAgent = Just "haskell:temp.switcharoo_scraper:v0.1.0" }

main :: IO ()
main = do
    return ()



switcharooPosts :: MonadIO m => RedditT m [Post]
switcharooPosts = subredditAll New (Just switcharooName)

subredditAll :: MonadIO m => ListingType -> Maybe SubredditName -> RedditT m [Post]
subredditAll lt sub = concat <$> subredditAll' Nothing where
    subredditAll' :: MonadIO m => Maybe (PaginationOption PostID) -> RedditT m [[Post]]
    subredditAll' pag = do
        Listing before' after' posts <- getPosts' (Options pag Nothing) lt sub
        case before' of Just b -> (posts :) <$> (subredditAll' (Just (After b)))
                        Nothing -> return [posts]

    

