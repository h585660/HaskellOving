module Week36Exercise2 where

{-
Semi-repetetive strenger har egenskaper:
                                          Hvis streng == partallslengde, første halvdel lik andre halvdel
                                          Hvis streng == oddetallslengde, delstrenger venstre/høyre for midt like
-}


semiRepetitive :: String -> Maybe String
decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
createSemiRepetitive :: String -> Maybe Char -> String


--Sjekker om streng s er semi-repetitiv, ekskluderer midterste bokstav om oddetallslengde
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



--Gjør samme som semiRepetitive, men returnerer også midterste bokstav
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


--Oppretter en semirepetitiv streng av input s, legger char ch som midterste hvis oddetallslengde ønsket
createSemiRepetitive s ch =
                            
                        if ch /= Nothing
                            then let Just char = ch in s++ [char] ++ s 
                            else s ++ s                           
                    







