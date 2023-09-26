module Week36Exercise2 where


semiRepetitive :: String -> Maybe String
decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
createSemiRepetitive :: String -> Maybe Char -> String


semiRepetitive s = 
                    let len = length s
                        mid = len `div` 2

                    in if len `mod` 2 == 0 
                        then let halv = drop mid s
                        in if halv == take mid s
                            then Just halv
                            else Nothing
                    else 
                        let forste =  take mid s
                            siste = drop (mid+1) s
                            
                            
                        in if forste == siste
                        then Just forste
                        else Nothing




decomposeSemiRepetitive s =
                let len = length s
                    mid = len `div` 2
                    
                    in if len `mod` 2 == 0
                        then let halv = drop mid s
                        in if halv == take mid s
                            then Just (halv, Nothing)
                            else Nothing
                            
                    else 
                        let forste = take mid s
                            siste = drop (mid+1) s
                            midt = s !! mid
                            
                        in if forste == siste
                            then Just (forste, Just midt)
                            else Nothing



createSemiRepetitive s ch =
                            
                        if ch /= Nothing
                            then let Just char = ch in s++ [char] ++ s 
                            else s ++ s                           
                    







