import System.Environment.FindBin (getProgPath)

import Data.List (sort)
import qualified Data.Set as Set

isValid :: String -> Bool
isValid [] = True
isValid passwords
    | Set.size wordset == length wordList = True
    | otherwise = False
    where wordList = words passwords
          wordset = Set.fromList(map sort wordList)

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
    print . sum $ [ 1 | passwords <- lines input, isValid passwords]
