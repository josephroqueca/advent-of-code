adjustFrequency :: Int -> [Int] -> Int
adjustFrequency = foldl (+)

stripPositives :: String -> String
stripPositives [] = []
stripPositives ('+':xs) = stripPositives xs
stripPositives (x:xs) = x : stripPositives xs

main :: IO()
main = do
    input <- readFile "../input.txt"
    putStr "The resulting frequency is: "
    print $ adjustFrequency 0 $ map read (lines $ stripPositives input)
