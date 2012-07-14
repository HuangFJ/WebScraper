{-# LANGUAGE OverloadedStrings #-}
module VOAParser where

import Text.HTML.TagSoup
import Text.Regex.Posix
import qualified Data.Text as T
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Set as Set
import qualified Data.Char as C (toLower)

cleanTags :: String -> String
cleanTags cnt = innerText $ mapTagList [] $ parseTags cnt

mapTagList :: [Tag String] -> [Tag String] -> [Tag String]
mapTagList tags (x:xs) = mapTagList tags' xs'
    where (tags', xs') = if tagOfDrop x 
                then (tags, dropTagList 1 xs)
                else (tags ++ [x], xs)
mapTagList tags [] = tags           

dropTagList :: Int -> [Tag String] -> [Tag String]
dropTagList deep (x:xs) = if deep' > 0 
    then dropTagList deep' xs 
    else xs
    where deep' = case x of
                TagOpen _ _ -> deep + 1
                TagClose _ -> deep - 1
                _ -> deep
dropTagList _ [] = []

likeTagAttrCls :: String -> Bool
likeTagAttrCls attrCls = attrCls =~ ("tagaudiotitle|mediaplayer|embedded_content_object|playlistlink|downloadlinkstatic|embeddedBackgrounderInner|listenlink|downloadico" :: String) :: Bool

likeTagName :: String -> Bool
likeTagName tagName = tagName =~ ("^(script|iframe)$" :: String) :: Bool

tagOfDrop :: Tag String -> Bool
tagOfDrop tag = case tag of
    TagOpen tagName _ -> (likeTagName tagName)
                      || (likeTagAttrCls $ fromAttrib "class" tag)
    _ -> False

voaFormat :: String -> String
voaFormat cnt = T.unpack $ T.unlines cleanLine
    where txtLine = T.lines $ T.pack $ cleanTags cnt
          cleanLine = filter (/= "") $ map T.strip txtLine

voaParse :: Int -> Int -> IO ()
voaParse start end = do
    conn <- connectSqlite3 "web_scrape.s3db"
    r <- quickQuery' conn "select id,body from voa_scrape where id between ? and ?" [toSql start, toSql end]
    mapM_ (\[sqlId, sqlBody] -> do
            let body_format = voaFormat $ fromSql sqlBody
                id' = fromSql sqlId :: Int
            run conn "update voa_scrape set body_format = ? where id = ?" [toSql body_format, toSql id']
        ) r
    commit conn
    disconnect conn

voaKeyWords :: Int -> Int -> IO ()
voaKeyWords start end = do
    conn <- connectSqlite3 "web_scrape.s3db"
    r <- quickQuery' conn "select id,body_format from voa_scrape where id between ? and ?" [toSql start, toSql end]
    lfcnt <- lowFreqContent
    mapM_ (\[sqlId, body_format] -> do
            let key_words = getKeyWords lfcnt $ fromSql body_format
                id' = fromSql sqlId :: Int
            run conn "update voa_scrape set key_words = ? where id = ?" [toSql key_words, toSql id']
        ) r
    commit conn
    disconnect conn

lowFreqContent :: IO String
lowFreqContent = readFile "low_frequency_words.txt"

getKeyWords :: String -> String -> String
getKeyWords lfcnt cnt = unwords $ Set.toList rSet
    where lfw = words lfcnt
          rSet = Set.filter (\x -> (map C.toLower x) `notElem` lfw) $ Set.fromList $ words cnt
    
    