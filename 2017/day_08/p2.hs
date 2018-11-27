import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

largestIntermediateRegisterValue :: [String] -> Map String Int -> Int
largestIntermediateRegisterValue [] registers = findLargestRegister registers (Map.keys registers)
largestIntermediateRegisterValue (x:xs) registers =
    if (operator == ">" && comparisonValue > comparator)
        || (operator == "<" && comparisonValue < comparator)
        || (operator == "!=" && comparisonValue /= comparator)
        || (operator == "==" && comparisonValue == comparator)
        || (operator == ">=" && comparisonValue >= comparator)
        || (operator == "<=" && comparisonValue <= comparator)
        then
            max (largestIntermediateRegisterValue xs $ Map.insert modRegister (performOperation (registerValue modRegister) modOperation modValue) registers) (findLargestRegister registers (Map.keys registers))
        else
            largestIntermediateRegisterValue xs registers
    where components = words x
          modRegister = head components
          modOperation = components !! 1
          modValue = (read::String->Int) $ components !! 2
          operator = components !! 5
          comparator = (read::String->Int) $ components !! 6
          registerValue register = if register `Map.member` registers then registers ! register else 0
          comparisonValue = registerValue $ components !! 4

performOperation :: Int -> String -> Int -> Int
performOperation originalValue operation modifier
    | operation == "inc" = originalValue + modifier
    | operation == "dec" = originalValue - modifier

findLargestRegister :: Map String Int -> [String] -> Int
findLargestRegister registers = foldr (\ x -> max (registers ! x)) 0

main :: IO()
main = do
    input <- readFile "input.txt"
    print $ largestIntermediateRegisterValue (lines input) Map.empty
