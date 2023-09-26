module Week38Exercise0 where

runningSum :: [Integer] -> [Integer]
runningSum xs = runningSumAcc xs 0
  where
    runningSumAcc [] _ = []
    runningSumAcc (x:xs) acc = let currentSum = x + acc in currentSum : runningSumAcc xs currentSum