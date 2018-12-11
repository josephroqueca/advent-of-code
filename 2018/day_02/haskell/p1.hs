import System.Environment.FindBin (getProgPath)

import Data.Map (Map, (!))
import qualified Data.Map as Map

hasRepeatedLetters :: String -> Map Char Int -> Int -> Bool
hasRepeatedLetters [] repeats goal = goal `elem` Map.elems repeats
hasRepeatedLetters (x:xs) repeats goal
    | xIsRepeated = hasRepeatedLetters xs (Map.insert x ((repeats ! x) + 1) repeats) goal
    | otherwise = hasRepeatedLetters xs (Map.insert x 1 repeats) goal
    where xIsRepeated = Map.member x repeats

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
    let ids = lines input
        twos = length (filter (\x -> hasRepeatedLetters x Map.empty 2) ids)
        threes = length (filter (\x -> hasRepeatedLetters x Map.empty 3) ids)
    putStr "The checksum is: "
    print $ twos * threes
