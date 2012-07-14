module Main where

import System.Environment (getArgs)
import StarDictParser
import VOAScraper
import VOAParser
import DictCNScraper

main::IO()
main = do
    args <- getArgs
    case args of
        ("starDictParse":file:_) -> starDictParse file
        ("voaScrape":file:_) -> voaScrape file
        ("dictScrape":_) -> dictScrape
        ("voaParse":s:e:_) -> voaParse (read s :: Int) (read e :: Int)
        _ -> do
            putStrLn "Useage: starDictParse <stardict_xml_file>"
            putStrLn "        dictScrape"
            putStrLn "        voaScrape <voa_feed_file>"
            putStrLn "        voaParse <start_id> <end_id>"
