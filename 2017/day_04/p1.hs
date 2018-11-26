import qualified Data.Set as Set

isValid :: String -> Bool
isValid [] = True
isValid passwords
    | Set.size wordset == length wordList = True
    | otherwise = False
    where wordList = words passwords
          wordset = Set.fromList wordList

main :: IO()
main = do
    input <- readFile "input.txt"
    print . sum $ [ 1 | passwords <- lines input, isValid passwords]
