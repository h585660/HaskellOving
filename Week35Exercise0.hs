module Week35Exercise0 where

    

f:: Integer -> Integer
f n = div (n*(n+1)) 2

main :: IO()
main = do
    print (f 2)

