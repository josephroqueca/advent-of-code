steps :: [Int] -> Int -> Int
steps offsets position = steps' offsets position 0

steps' :: [Int] -> Int -> Int -> Int
steps' offsets position stepsSoFar
    | position <= lowerBound || position >= upperBound = stepsSoFar
    | otherwise = let newOffsets = take position offsets ++ [currentOffset + 1] ++ drop (position + 1) offsets in
        steps' newOffsets (position + currentOffset) (stepsSoFar + 1)
    where totalOffsets = length offsets
          currentOffset = offsets !! position
          lowerBound = -1
          upperBound = totalOffsets


main :: IO()
main = do
    input <- readFile "../input.txt"
    let offsets = map (read::String -> Int) (lines input)
    print $ steps offsets 0
