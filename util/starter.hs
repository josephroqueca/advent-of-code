import System.Environment.FindBin (getProgPath)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

main :: IO()
main = do
    scriptDir <- getProgPath
    input <- readFile (scriptDir ++ "/../input.txt")
