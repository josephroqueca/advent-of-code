import Data.List (maximumBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord (Ordering)
import Data.Set (Set)
import qualified Data.Set as Set

countRedistributions :: Map Int Int -> Int
countRedistributions memoryMap = countRedistributions' memoryMap (Set.fromList [show . Map.elems $ memoryMap]) 0

countRedistributions' :: Map Int Int -> Set String -> Int -> Int
countRedistributions' memoryMap pastConfigurations totalRedistributions
    | redistribution' `elem` pastConfigurations = totalRedistributions + 1
    | otherwise = countRedistributions' nextMemoryMap nextConfigurations (totalRedistributions + 1)
    where redistribution = redistribute memoryMap
          redistribution' = show redistribution
          nextMemoryMap = mapMemory redistribution
          nextConfigurations = Set.insert redistribution' pastConfigurations

bankWithMostBlocks :: Map Int Int -> Int
bankWithMostBlocks memoryMap = let (idx, _) = maximumBy compareBanks (Map.assocs memoryMap) in idx

compareBanks :: (Int, Int) -> (Int, Int) -> Ordering
compareBanks first second
    | ordering == GT = GT
    | ordering == LT = LT
    | otherwise = if fst first > fst second then LT else GT
    where ordering = compare (snd first) (snd second)

redistribute :: Map Int Int -> [Int]
redistribute memoryMap = redistribute' (Map.insert redistributedBank 0 memoryMap) (nextBank redistributedBank memoryMap) (memoryMap ! redistributedBank)
    where redistributedBank = bankWithMostBlocks memoryMap

redistribute' :: Map Int Int -> Int -> Int -> [Int]
redistribute' memoryMap position remainingBlocks
    | remainingBlocks == 0 = Map.elems memoryMap
    | otherwise = redistribute' (Map.insert position ((memoryMap ! position) + 1) memoryMap) nextPosition (remainingBlocks - 1)
    where nextPosition = nextBank position memoryMap

nextBank :: Int -> Map Int Int -> Int
nextBank position memoryMap = if position + 1 < Map.size memoryMap then position + 1 else 0

mapMemory :: [Int] -> Map Int Int
mapMemory memoryBanks = Map.fromList $ zip [0..length memoryBanks] memoryBanks

main :: IO()
main = do
    input <- readFile "input.txt"
    let inputAsIntegers = map (read::String -> Int) (words input) in
        print . countRedistributions . mapMemory $ inputAsIntegers
