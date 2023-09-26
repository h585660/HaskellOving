module Week37Exercise1 where

--Sjekker om ligning a^m + b^m == c^(m-1) har en løsning for heltall hvor 1 <= a <= b <= c && m >= 1

semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]

semiFermat n m = [(a,b,c) |
                a <- [1..n],
                b <- [a..n],
                c <- [b..n],
                a^m + b^m == c^(m-1)]
            
main :: IO ()
main = do
    print $ semiFermat 20 1
