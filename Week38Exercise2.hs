module Week38Exercise2 where

--Fjerner alle elementer av typ Nothing ved hjelp av rekursjon

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (x:xs) =
  case x of
    Just val -> val : removeNothing xs
    Nothing  -> removeNothing xs
