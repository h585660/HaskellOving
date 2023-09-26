module Week38Exercise1 where


combinations :: Integer -> [Char] -> [String]
combinations 0 _ = [""]
combinations n alphabet = [c:s | c <- alphabet, s <- combinations (n - 1) alphabet]