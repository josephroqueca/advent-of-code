import Data.Map (Map, fromList, insert, size, (!))

stepThrough :: Map Int Int -> Int -> Int
stepThrough offsets position = stepThrough' offsets position 0

stepThrough' :: Map Int Int -> Int -> Int -> Int
stepThrough' offsets position stepsSoFar
    | position <= lowerBound || position >= upperBound = stepsSoFar
    | otherwise = let newOffsets = insert position adjustedOffset offsets in stepThrough' newOffsets (position + currentOffset) (stepsSoFar + 1)
    where currentOffset = offsets ! position
          adjustedOffset = if currentOffset >= 3 then currentOffset - 1 else currentOffset + 1
          lowerBound = -1
          upperBound = size offsets

main :: IO()
main = do
    input <- readFile "../input.txt"
    let offsets = map (read::String -> Int) (lines input)
    let offsetMap = fromList $ zip [0..(length offsets)] offsets
    print $ stepThrough offsetMap 0
