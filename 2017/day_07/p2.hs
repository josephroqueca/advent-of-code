import Debug.Trace (trace)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

data Program = Program { programName :: String
                       , programWeight :: Int
                       , heldPrograms :: [String] } deriving (Show)

buildProgram :: String -> Program
buildProgram input
    | length splitInput == 2 =
        let [name,parenthesesWeight] = splitInput in
        Program name (extractProgramWeight parenthesesWeight) []
    | otherwise =
        let (name:parenthesesWeight:_:subPrograms) = splitInput in
        Program name (extractProgramWeight parenthesesWeight) (map extractSubProgram subPrograms)
    where splitInput = words input

extractSubProgram :: String -> String
extractSubProgram [] = ""
extractSubProgram [','] = ""
extractSubProgram (x:xs) = x : extractSubProgram xs

extractProgramWeight :: String -> Int
extractProgramWeight parenthesesWeight = read $ extractProgramWeight' parenthesesWeight

extractProgramWeight' :: String -> String
extractProgramWeight' [] = ""
extractProgramWeight' [')'] = ""
extractProgramWeight' ('(':xs) = extractProgramWeight' xs
extractProgramWeight' (x:xs) = x : extractProgramWeight' xs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ foldr (++) [] xs

weightOfSubTowers :: Map String Program -> Program -> Int
weightOfSubTowers programMap program
    | totalSubTowers == 0 = programWeight program
    | otherwise =
        let getSubWeights = weightOfSubTowers programMap
            getProgram x = programMap ! x
            subTowerWeights = map (getSubWeights . getProgram) subTowers
            totalSubTowerWeight = foldl (+) 0 subTowerWeights
            subTowerWeightsEqual = totalSubTowerWeight `div` length subTowerWeights == head subTowerWeights
            pWeights = processWeights subTowerWeightsEqual subTowerWeights (programWeight program) in
            programWeight program + totalSubTowerWeight + pWeights
    where subTowers = heldPrograms program
          totalSubTowers = length subTowers

processWeights :: Bool -> [Int] -> Int -> Int
processWeights True _ _ =
    -- trace (show weights)
    0
processWeights False weights pW =
    trace (show weights)
    trace (show pW)
    0

main :: IO()
main = do
    input <- readFile "input.txt"
    let programs = map buildProgram (lines input)
    let programMap = Map.fromList $ zip (map programName programs) programs
    let subPrograms = Set.fromList . flatten $ map heldPrograms (Map.elems programMap)
    let notMember x = Set.notMember x subPrograms
        [baseProgram] = filter notMember (Map.keys programMap) in
        print $ weightOfSubTowers programMap (programMap ! baseProgram)
