{-# LANGUAGE OverloadedStrings #-}
{-
解析星级译王的xml词典文件
CREATE TABLE [dictcn_scrape] ( 
[proto] VARCHAR( 332 ) PRIMARY KEY NOT NULL,
[json] TEXT NOT NULL DEFAULT (''),
[definition] TEXT NOT NULL DEFAULT ('')
);
-}
module StarDictParser (starDictParse) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.UTF8 as LU
import Text.XML.Expat.SAX
import qualified Control.Exception as E
import Database.HDBC.Sqlite3
import Database.HDBC

type KeyValue = (String, String)

kvNode r (StartElement "key" _:xs) = do
    let (key, xs') = takeKey "" xs
        (definition, xs'') = definitionNode xs'
    kvNode (r ++ [(key,definition)]) xs''
kvNode r (_:xs) = kvNode r xs
kvNode kvs [] = kvs

takeKey w (CharacterData l:xs) = takeKey (w ++ l) xs
takeKey key (EndElement "key":xs) = (key, xs)

definitionNode (StartCData:xs) = takeDefinition "" xs
definitionNode (_:xs) = definitionNode xs

takeDefinition w (CharacterData l:xs) = takeDefinition (w ++ l) xs
takeDefinition definition (EndCData:xs) = (definition, xs)

insertDB conn (k,v) = E.catch (do
        -- LC.writeFile ("trans/" ++ k) $ LU.fromString value
        run conn "insert into dictcn_scrape (proto,definition) values (?,?)" [toSql k, toSql v]
        return ()
    ) handler
    where
        handler e = print (e :: E.SomeException)

starDictParse :: String -> IO ()
starDictParse xmlfile = do
    -- 输出中文
    LC.putStrLn $ LU.fromString "开始解析"
    rawContent <- L.readFile xmlfile
    let parsed = kvNode [] (parse defaultParseOptions rawContent :: [SAXEvent String String])
    conn <- connectSqlite3 "web_scrape.s3db"
    mapM_ (insertDB conn) parsed
    commit conn
    disconnect conn
    putStrLn "Complete!"
 
