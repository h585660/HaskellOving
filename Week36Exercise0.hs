module Week36Exercise0 where

f:: String -> Char -> Bool

f str c = not (c `elem` str) 


{- main :: IO ()
main = do
    let myString = "Hallo hallo"
    let myChar = 'd'
    
    print (f myString myChar) -}



    