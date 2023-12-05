module Week43Exercise2 where

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

data MyGraph = MyGraph [Integer] [(Integer, Integer)]
  deriving Show

instance IntegerGraph MyGraph where
    emptyGraph = MyGraph [] []

    insertNode node (MyGraph nodes edges) =
      if node `elem` nodes
      then MyGraph nodes edges
      else MyGraph (node:nodes) edges

    insertEdge from to graph =
        let MyGraph updatedNodes updatedEdges = insertNode from $ insertNode to graph
        in if (from, to) `elem` updatedEdges
          then MyGraph updatedNodes updatedEdges
          else MyGraph updatedNodes ((from, to):updatedEdges)

    nodeInGraph node (MyGraph nodes _) = node `elem` nodes

    edgeInGraph from to (MyGraph _ edges) = (from, to) `elem` edges

graph :: (IntegerGraph g) => g
graph = insertEdge 1 6 $
        insertEdge 1 8 $
        insertEdge 5 1 $
        insertEdge 5 8 $
        insertEdge 8 5 $
        insertNode 3 $
        emptyGraph

