module Week36Exercise0 where

--Oppgaven krevde bare å lage en funksjon av typ String -> Char -> Bool

f:: String -> Char -> Bool

f str c = not (c `elem` str) 


{- main :: IO ()
main = do
    let myString = "Hallo hallo"
    let myChar = 'd'
    
    print (f myString myChar) -}



    
