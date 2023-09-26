module Week37Exercise0 where


information :: [String] -> [String] -> [Integer] -> [String] 


information a b c =
    let zipped = zip3 a b c
        filtered = filter (\(_, _, year) -> year >= 2022) zipped
        stringTuples = map myFunction filtered
    in stringTuples
    where 
        myFunction :: (String, String, Integer) -> String
        myFunction (x, y, z) =  x ++ " is studying at " ++ y ++ " department and started in " ++ show z 
xs :: [String]
xs = ["Farhad", "Tina", "Alex"]
xy :: [String] 
xy = ["Informatics", "Biological Sciences", "Mathematics"]
xp :: [Integer]
xp = [2022, 2023, 2019]
    
main :: IO ()
main = do
    print (information xs xy xp)
    