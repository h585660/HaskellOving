module Week36Exercise1 where


--Oppgaven krevde bare funksjon av typ [Integer] -> [t] -> [(Integer, t)] og bruk av Reverse/Zip

f :: [Integer] -> [t] -> [(Integer, t)]

f x y = zip (reverse x) y

{- main :: IO ()
main = do
    let a = [1,2,3]
    let b = [1,4,5]

    print (f a b) -}
