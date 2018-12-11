import System.Environment.FindBin (getProgPath)

adjustFrequency :: Int -> [Int] -> Int
adjustFrequency = foldl (+)

stripPositives :: String -> String
stripPositives [] = []
stripPositives ('+':xs) = stripPositives xs
stripPositives (x:xs) = x : stripPositives xs

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
    putStr "The resulting frequency is: "
    print $ adjustFrequency 0 $ map read (lines $ stripPositives input)
