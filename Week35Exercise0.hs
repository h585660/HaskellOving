module Week35Exercise0 where


--Funksjon f som regner n*(n+1)/2 på parameter n

f:: Integer -> Integer
f n = div (n*(n+1)) 2

main :: IO()
main = do
    print (f 2)

