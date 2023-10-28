module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing, listToMaybe, mapMaybe)

type Graph n = Map n (Set n)

bridge :: (Ord n) => n -> n -> Graph n -> Graph n
bridge node1 node2 myGraph
    | isNothing (path myGraph node1 node2) = 
        Map.insertWith Set.union node1 (Set.singleton node2) (ensureNode node2 (ensureNode node1 myGraph))
    | otherwise = myGraph
    where
        ensureNode thisNode graphy = Map.insertWith Set.union thisNode Set.empty graphy

path :: (Ord node) => Graph node -> node -> node -> Maybe [node]
path g start end = path' g start end Set.empty

path' :: (Ord node) => Graph node -> node -> node -> Set node -> Maybe [node]
path' g start end visited
    | Set.member start visited = Nothing -- We have reached a cycle
    | start == end = Just []
    | otherwise = do
        let visited' = Set.insert start visited
        nexts <- Map.lookup start g
        listToMaybe $ mapMaybe (\next -> do
                                    pathCont <- path' g next end visited'
                                    Just (next:pathCont))
                                (Set.toList nexts)
