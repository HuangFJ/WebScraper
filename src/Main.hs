module Main where

import System.Environment (getArgs)
import StarDictParser
import VOAScraper
import DictCNScraper

main::IO()
main = do
    args <- getArgs
    case args of
        ("starDictParse":file:_) -> starDictParse file
        ("voaScrape":file:_) -> voaScrape file
        ("dictScrape":_) -> dictScrape
        _ -> do
            putStrLn "Useage: starDictParse <stardict_xml_file>"
            putStrLn "        voaScrape <voa_feed_file>"
            putStrLn "        dictScrape"
