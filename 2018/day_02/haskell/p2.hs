import System.Environment.FindBin (getProgPath)

countDifferingLetters :: String -> String -> Int -> Int
countDifferingLetters [] [] differing = differing
countDifferingLetters (x:xs) (y:ys) differing
    | x == y = countDifferingLetters xs ys differing
    | otherwise = countDifferingLetters xs ys (succ differing)
countDifferingLetters _ _ _ = error "Invalid ids!"

dropDifferentLetters :: String -> String -> String
dropDifferentLetters [] [] = []
dropDifferentLetters (x:xs) (y:ys)
    | x == y = x : dropDifferentLetters xs ys
    | otherwise = dropDifferentLetters xs ys
dropDifferentLetters _ _ = error "Invalid ids!"

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
    let input_lines = lines input
    let similar_ids = filter (\x -> not (null (filter (\y -> countDifferingLetters x y 0 == 1) input_lines))) input_lines
    putStr "The two similar IDs are: "
    print similar_ids
    putStr "The common characters are: "
    print $ dropDifferentLetters (head similar_ids) (head $ tail similar_ids)
