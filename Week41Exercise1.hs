module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint set1 set2 = Set.null (Set.intersection set1 set2)

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle graph node = 
    let 
        checkCycle visitedNodes currentNode = 
            case Map.lookup currentNode graph of
                Nothing -> False
                Just childrenNodes -> 
                    if not (disjoint visitedNodes childrenNodes)
                        then True 
                        else any (checkCycle (Set.insert currentNode visitedNodes)) (Set.toList childrenNodes)
    in 
        checkCycle (Set.singleton node) node