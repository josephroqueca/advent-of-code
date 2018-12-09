import qualified Data.Map as Map
import qualified Data.Set as Set

data Program = Program { programName :: String
                       , programWeight :: Int
                       , heldPrograms :: [String] } deriving (Show)

buildProgram :: String -> Program
buildProgram input
    | length splitInput == 2 =
        let [name,parenthesesWeight] = splitInput in
            let weight = extractProgramWeight parenthesesWeight in Program name weight []
    | otherwise =
        let (name:parenthesesWeight:_:subPrograms) = splitInput in
            let weight = extractProgramWeight parenthesesWeight in Program name weight (map extractSubProgram subPrograms)
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

-- createTree :: Program -> Map String Program -> Tree Program

main :: IO()
main = do
    input <- readFile "../input.txt"
    let programs = map buildProgram (lines input)
    let programMap = Map.fromList $ zip (map programName programs) programs
    let subPrograms = Set.fromList . flatten $ map heldPrograms (Map.elems programMap)
    let notMember x = Set.notMember x subPrograms in
        let baseProgram = filter notMember (Map.keys programMap) in
            print baseProgram
