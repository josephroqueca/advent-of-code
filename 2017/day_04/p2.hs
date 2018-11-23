import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

import System.IO

isValid :: String -> Bool
isValid [] = True
isValid passwords
    | Set.size wordset == length wordList = True
    | otherwise = False
    where wordList = words passwords
          wordset = Set.fromList(map sort wordList)

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn . show . sum $ [ 1 | passwords <- (lines input), isValid passwords]
