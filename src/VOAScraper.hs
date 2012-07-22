{-# LANGUAGE OverloadedStrings #-}

module VOAScraper (voaScrape) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad.Maybe
import qualified Control.Exception as E
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Time
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.ParallelIO
import Text.Regex.Posix
import Codec.Binary.UTF8.String (decodeString)
import VOAParser

{-
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Control.Monad

sleepMinute :: Int -> IO ()
sleepMinute min = do 
    t <- getCurrentTime
    let secs = round (realToFrac $ utctDayTime t) `rem` (60 * min)
    threadDelay $ 1000000 * ((60 * min) - secs)
-}

explode :: (a -> Bool) -> [a] -> [[a]]
explode _ [] = []
explode f xs
    | null zs = [z]
    | null z  = explode f (tail zs)
    | otherwise    = z : explode f (tail zs)
    where (z, zs) = break f xs

explode' :: Char -> String -> [String]
explode' sp = explode (== sp)

head' :: [String] -> String
head' xs = case xs of
    [] -> ""
    (x:_) -> x

-- 解析voa的播放器弹出框，获取多媒体下载地址
getMediaUrl :: String -> IO String
getMediaUrl url = do
    mediaContent <- runMaybeT $ openUrl url
    case mediaContent of
        Nothing -> return ""
        Just mediaContent' -> do
            let doc = readString [withParseHTML yes, withWarnings no] $ decodeString mediaContent'
            mediaURLs <- runX $ doc >>> css "div" 
                                    >>> hasAttrValue "id" (== "flash_audio") 
                                    >>> css "a" 
                                    >>> hasAttrValue "class" (== "listenico") ! "href"
            case mediaURLs of
                [] -> return ""
                (x:_) -> return x
                
-- 解析voa的文章页面，如果多媒体在文章内则直接获取地址，如果是弹出框则从弹出框中获取
getContent :: String -> IO (String, String)
getContent url = do
    putStrLn $ "parsing: " ++ url
    content <- runMaybeT $ openUrl url
    case content of
        Nothing -> return ("", "")
        Just content' -> do
            let doc = readString [withParseHTML yes, withWarnings no] $ decodeString content'
            mediaURLs <- runX $ doc >>> css "div#article" 
                                    -- >>> css "div.zoomMe" 
                                    >>> css "a.downloadico" ! "href"
                                    
            let mediaURL = unsafePerformIO $ case mediaURLs of
                    [] -> do
                        listenURLs <- runX $ doc >>> css "div#article" 
                                                 >>> css "li.listenlink" 
                                                 >>> css "a.listenico" ! "href"
                        case listenURLs of 
                            [] -> return ""
                            (x:_) -> do
                                let (_ , _ , _ , matchs) = url =~ ("(http://[^/]+)" :: String) :: (String,String,String,[String])
                                getMediaUrl $ head' matchs ++ x
                    (x:_) -> return x

            rawdiv <- runX . xshow $ doc >>> css "div#article" 
                                         >>> css "div.zoomMe"
            return (mediaURL, head' rawdiv)

-- 将链表转为sql格式   
concatSql :: (String, String, [String]) -> [(String, String)] -> (String, String, [String])
concatSql cond (x:xs) = concatSql cond' xs
    where (sql, sql2, value) = cond
          (key, val) = x
          cond' = case key of
            "title" -> 
                (sql ++ "title,", sql2 ++ "?,", value ++ [val])
            "guid" -> 
                (sql ++ "uri,", sql2 ++ "?,", value ++ [val])
            "description" -> 
                (sql ++ "description,", sql2 ++ "?,", value ++ [val])
            "pubDate" -> 
                (sql ++ "pub_date,", sql2 ++ "?,", value ++ [val])
            "category" -> 
                (sql ++ "category,", sql2 ++ "?,", value ++ [val])
            "author" -> 
                (sql ++ "author,", sql2 ++ "?,", value ++ [val])
            "enclosure" -> 
                (sql ++ "enclosure,", sql2 ++ "?,", value ++ [val])
            _ -> (sql, sql2, value)
concatSql cond [] = cond

-- 将数据写入到sqlite3数据库
insertDB :: [(String, String)] -> IO ()
insertDB x = E.catch (do
    let uri = Map.fromList x Map.! "guid"
    conn <- connectSqlite3 "web_scrape.s3db"
    r <- quickQuery' conn "select id from voa_scrape where uri=?" [toSql uri]
    disconnect conn
    lfcnt <- lowFreqContent
    case r of
        [] -> do 
            let (sql, sql2, value) =  concatSql ("","",[]) x
                
            cnt <- getContent uri
            let (media_url, body) = cnt
                (body', body_format', key_words', timestamp') = case media_url of
                    "" -> ("", "", "", 0)
                    _ -> (body, body_format, key_words, timestamp)
                        where body_format = voaFormat body
                              key_words = getKeyWords lfcnt body_format
                              timestamp = unsafePerformIO $ getClockTime >>= (\(TOD sec _) -> return sec)
                sql' = sql ++ "media_url,body,body_format,key_words,ctime"
                sql2' = sql2 ++ "?,?,?,?,?"
                value' = value ++ [media_url] ++ [body'] ++ [body_format']
                               ++ [key_words'] ++ [show timestamp']
            
            conn' <- connectSqlite3 "web_scrape.s3db"
            run conn' ("insert into voa_scrape (" ++ sql' ++ ") values (" ++ sql2' ++ ")") (map toSql value')
            commit conn'
            disconnect conn'
        _ -> putStrLn $ "ignore: " ++ uri 
    ) handler
    where handler e = print (e :: E.SomeException)

-- 从voa的rss抓取文章地址        
scrapeXML :: String -> IO ()
scrapeXML url = do
    putStrLn $ "feed: " ++ url
    content <- runMaybeT $ openUrl url
    case content of
        Nothing -> putStrLn $ "error: " ++ url
        Just content' -> do
            --titles <- runX $ doc >>> getChildren >>> getChildren >>> getChildren >>> hasName "title" >>> getChildren >>> getText
            let doc = readString [] content'
            xs <- runX $ doc >>> css "item" 
                             >>> (css "title" >>> getName &&& deep getText)
                             <+> (css "guid" >>> getName &&& deep getText)
                             <+> (css "description" >>> getName &&& deep getText)
                             <+> (css "pubDate" >>> getName &&& deep getText)
                             <+> (css "category" >>> getName &&& deep getText)
                             <+> (css "author" >>> getName &&& deep getText)
                             <+> (css "enclosure" >>> getName &&& getAttrValue "url")
            mapM_ insertDB (groupItem xs [] [])
            where groupItem ((key, val):xs) item allItems = if key == "title" 
                        then groupItem xs [(key, val)] (item:allItems)
                        else groupItem xs ((key, val):item) allItems
                  groupItem [] item allItems = init (item:allItems)

voaScrape :: String -> IO ()
voaScrape feedfile = do
    rawContent <- readFile feedfile
    parallel_ $ map scrapeXML $ explode' '\n' rawContent
    stopGlobalPool

