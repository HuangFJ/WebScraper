{-# LANGUAGE OverloadedStrings #-}

{-
海词例句抓取
CREATE TABLE [dictcn_scrape] ( 
[proto] VARCHAR( 332 ) PRIMARY KEY NOT NULL,
[json] TEXT NOT NULL DEFAULT ''
);
-}
module DictCNScraper where

import Codec.Binary.UTF8.String (decodeString)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad.Maybe
import Text.Regex.Posix
import Data.Bits
import Data.Char
import qualified Control.Exception as E
import Control.Concurrent.ParallelIO
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.HTTP.Base
import Text.JSON

head' :: [String] -> String
head' xs = case xs of
        [] -> ""
        (x:_) -> x

--获取和解析海词字典 开始
getContent :: String -> IO ()
getContent word = E.catch (do
    print $ "parsing '" ++ word ++ "'..."
    content <- runMaybeT $ openUrl $ "http://dict.cn/" ++ urlEncode word
    case content of
        Nothing -> putStrLn "can't get page from dict.cn"
        Just content' -> do
            let doc = readString [withParseHTML yes, withWarnings no] $ decodeString content'
            --rawdiv <- runX . xshow $ doc >>> css "div#cy"
            tokenjssrc <- runX $ doc >>> css "script"
                                     >>> hasAttrValue "id" (== "token-js") ! "src"
            tokenjs <- runMaybeT $ openUrl $ "http://dict.cn" ++ head' tokenjssrc
            case tokenjs of
                Nothing -> putStrLn "can't get key from dict.cn"
                Just tokenjs' -> case getDictData content' of
                        Nothing -> putStrLn "get data error"
                        Just dictdata -> do
                            let dictkey = getDictKey tokenjs'
                                result = decrypt dictkey 0 dictdata
                            updateDB word result
                            print $ "'" ++ word ++ "' ok"
    ) handler
    where
        handler e = print (e :: E.SomeException)


getDictKey :: String -> Int
getDictKey tokenjs = do
    let (_ , _ , _ , matchs) = tokenjs =~ ("\\$dict_key=([^;]+);" :: String) :: (String,String,String,[String])
    read (head' matchs) :: Int


getDictData :: String -> Maybe String
getDictData datajs = do
    let (_ , _ , _ , datamatchs) = datajs =~ ("\\$dict_data  = (\".+[^\\]\");" :: String) :: (String,String,String,[String])
        datajs' = decode (head' datamatchs) :: Result JSString
    case datajs' of
        Error _ -> Nothing
        Ok jsvalue -> Just (fromJSString jsvalue)


decrypt :: Int -> Int -> String -> String
decrypt _ _ [] = []
decrypt dictkey i (char:xs) = do
    let c = ord char
        code = case i `mod` 2 of
            0 -> [chr $ (complement c) .&. 0x7f]
            _ -> [chr $ c `xor` dictkey]
    code ++ (decrypt dictkey (i+1) xs)
--获取和解析海词字典 结束

getAllWords :: IO [String]
getAllWords = do
    conn <- connectSqlite3 "web_scrape.s3db"
    r <- quickQuery' conn "select proto from dictcn_scrape where json = ''" []
    disconnect conn
    return $ map (\x -> fromSql $ head x :: String) r

updateDB :: String -> String -> IO ()
updateDB proto json = do
    conn <- connectSqlite3 "web_scrape.s3db"
    run conn "update dictcn_scrape set json = ? where proto = ?" [toSql json, toSql proto]
    commit conn
    disconnect conn

main :: IO ()
main = do
    allWords <- getAllWords
    parallel_ $ map getContent allWords
    stopGlobalPool

