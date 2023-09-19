module Main (
    main
    ) where

import Control.Concurrent.CachedIO (cachedIO, Cached(..))
import Data.List (isInfixOf)

crawlTheInternet :: IO [String]
crawlTheInternet = do
    putStrLn "Scanning pages.."
    putStrLn "Parsing HTML.."
    putStrLn "Following links.."
    return ["website about Haskell", "website about Ruby", "slashdot.org",
            "The Monad.Reader", "haskellwiki"]

searchEngine :: String -> Cached IO [String] -> IO [String]
searchEngine query internet = do
    pages <- runCached internet
    return $ filter (query `isInfixOf`) pages

main :: IO ()
main = do
    cachedInternet <- cachedIO 600 crawlTheInternet   -- 10 minute cache
    print =<< searchEngine "Haskell" cachedInternet
    print =<< searchEngine "C#" cachedInternet
