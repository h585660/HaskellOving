module Week40Exercise0 where

data SemiRepetitive = SemiRepetitive String (Maybe Char) deriving (Show, Eq)

semiRepetitive :: String -> Maybe SemiRepetitive
semiRepetitive s
    | length s == 0 = Just (SemiRepetitive "" Nothing)
    | length s == 1 = Just (SemiRepetitive "" (Just (head s)))
    | mod (length s) 2 == 0 && left == right = Just (SemiRepetitive left Nothing)
    | mod (length s) 2 /= 0 && left == oddRight = Just (SemiRepetitive left (Just mid))
    | otherwise = Nothing
    where
        halfLength = length s `div` 2
        left = take halfLength s
        right = drop halfLength s
        oddRight = drop 1 right
        mid = head right
        


toString :: SemiRepetitive -> String
toString (SemiRepetitive half Nothing)  
    | length half == 0 = half
    | otherwise = half ++ half
toString (SemiRepetitive half (Just mid)) = half ++ [mid] ++ half
