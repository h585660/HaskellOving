module Week42Exercise2 where

isFiveMultiples :: [Integer] -> Bool
isFiveMultiples = all ((== 0) . (`mod` 5))


factorial :: Integer -> Integer
factorial = foldr (*) 1 . enumFromTo 1

