{-# LANGUAGE FlexibleContexts #-}

module SwitchCrawler.Parse 
( doParsePermalink
, parsePermalink
, doScrapePermalinks
, scrapePermalinks
) where

import Control.Monad.Identity
import Data.Char (digitToInt)
import Data.Text (Text, pack)
import Reddit.Types.Comment
import Reddit.Types.Post
import Text.Parsec
import Text.Parsec.Char

doScrapePermalinks :: Stream s Identity Char => s -> Either ParseError [(String, PostID, CommentID, Int)]
doScrapePermalinks s = parse scrapePermalinks "post content" s



scrapePermalinks :: Stream s m Char => ParsecT s u m [(String, PostID, CommentID, Int)]
scrapePermalinks = do
    links <- many $ try $ do 
        many $ noneOf "["
        char '['
        text <- many $ noneOf "[]()"
        char ']'
        spaces
        char '('
        spaces
        (post, comment, id) <- try parsePermalink
        spaces
        char ')'
        return (text, post, comment, id)
    many anyChar
    return links
    

doParsePermalink :: Stream s Identity Char => s -> Either ParseError (PostID, CommentID, Int)
doParsePermalink s = parse parsePermalink "permalink" s

parsePermalink :: Stream s m Char => ParsecT s u m (PostID, CommentID, Int)
parsePermalink = do
    --optional reddit domain, may just be a /r/... link
    optional $ try $ do 
        --http with optional https
        string "http"
        optional (char 's')
        string "://"
        (try (string "reddit.com") <|> (many alphaNum >> char '.' >> string "reddit.com"))
    --optional subreddit name
    optional $ try $ string "/r/" >> many (noneOf [' ', '/'])
    string "/comments/"
    post <- many alphaNum
    --post name
    char '/'
    --
    many (noneOf [' ', '/'])
    char '/'
    --comment name
    comment <- many alphaNum
    optional $ char '/'
    context <- option 0 (try $ string "?context=" >> decimal) 
    many $ noneOf ")"
    return (PostID (pack post), CommentID (pack comment), context) where
        decimal = do
            digits <- many1 baseDigit
            let n = foldl (\x d -> base*x + (digitToInt d)) 0 digits
            seq n (return n)
        base = 10
        baseDigit = digit 
    
    
    
