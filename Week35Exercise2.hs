module Week35Exercise2 where

--Reverserer liste konkatenert

palindrome :: [a] -> [a]

palindrome xs = xs ++ reverse xs
                            
