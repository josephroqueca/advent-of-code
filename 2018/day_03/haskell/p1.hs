import System.Environment.FindBin (getProgPath)
import Debug.Trace (trace)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, reverse)

readClaim :: String -> String -> [String] -> [Int]
readClaim [] inProgress completed
  | null inProgress = 
    trace (show completed)
    map (read::String -> Int) (reverse completed)
  | otherwise = map (read::String -> Int) (reverse (reverse inProgress : completed))
readClaim (x:xs) inProgress completed
  | x >= '0' && x <= '9' =
    trace (show inProgress)
    readClaim xs (x : inProgress) completed
  | otherwise = readClaim xs [] (inProgress : completed)

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
    print $ map (\x -> readClaim x [] []) (lines input)
