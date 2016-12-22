{-# LANGUAGE FlexibleContexts #-}

module SwitchCrawler.Parse 
( doParsePermalink
, parsePermalink
, doScrapePermalinks
, scrapePermalinks
) where

import Data.Char (digitToInt)
import Data.Text (Text, pack)
import Reddit.Types.Comment
import Reddit.Types.Post
import Text.Parsec
import Text.Parsec.Char

doScrapePermalinks :: String -> Either ParseError [(String, PostID, CommentID, Int)]
doScrapePermalinks s = parse scrapePermalinks "post content" s

scrapePermalinks :: Stream s m Char => ParsecT s u m [(String, PostID, CommentID, Int)]
scrapePermalinks = do
    links <- many $ try $ do 
        many $ noneOf "["
        char '['
        text <- many $ noneOf "[]()"
        char ']'
        char '('
        (post, comment, id) <- try parsePermalink
        char ')'
        return (text, post, comment, id)
    many anyChar
    return links
    

doParsePermalink :: String -> Either ParseError (PostID, CommentID, Int)
doParsePermalink s = parse parsePermalink "permalink" s

parsePermalink :: Stream s m Char => ParsecT s u m (PostID, CommentID, Int)
parsePermalink = do
    --http with optional https
    string "http"
    optional (char 's')
    string "://"
    optional (string "www.")
    string "reddit.com"
    --optional subreddit name
    optional (string "/r/" >> many (noneOf [' ', '/']))
    string "/comments/"
    post <- many alphaNum
    --post name
    char '/'
    many (noneOf [' ', '/'])
    char '/'
    comment <- many alphaNum
    context <- option 0 (string "?context=" >> decimal) 
    many $ noneOf ")"
    return (PostID (pack post), CommentID (pack comment), context) where
        decimal = do
            digits <- many1 baseDigit
            let n = foldl (\x d -> base*x + (digitToInt d)) 0 digits
            seq n (return n)
        base = 10
        baseDigit = digit 
    
    
    
