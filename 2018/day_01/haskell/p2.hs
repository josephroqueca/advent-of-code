import Data.Set (Set)
import qualified Data.Set as Set

adjustFrequency :: Set Int -> Int -> [Int] -> [Int] -> Int
-- adjustFrequency = foldl (+)
adjustFrequency freqSet freq remainingAdjustments originalAdjustments
    | Set.member freq freqSet = freq
    | null remainingAdjustments = adjustFrequency freqSet freq originalAdjustments originalAdjustments
    | otherwise =
        adjustFrequency (Set.insert freq freqSet) updatedFrequency (drop 1 remainingAdjustments) originalAdjustments
    where updatedFrequency = freq + head remainingAdjustments

stripPositives :: String -> String
stripPositives [] = []
stripPositives ('+':xs) = stripPositives xs
stripPositives (x:xs) = x : stripPositives xs

main :: IO()
main = do
    input <- readFile "../input.txt"
    putStr "The first repeated frequency is: "
    let adjustments = map (read::String -> Int) (lines $ stripPositives input) in
        print $ adjustFrequency Set.empty 0 adjustments adjustments
