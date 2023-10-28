module Week42Exercise0 where

applyFunctions :: [a -> b] -> [b -> c] -> [a] -> [c]
applyFunctions functionList1 functionList2 valueList = 
    
    if null functionList1 || null functionList2 || null valueList

    then []
    else 

        let firstFunction1 = head functionList1
            firstFunction2 = head functionList2
            firstValue = head valueList

            resultValue = firstFunction2 (firstFunction1 firstValue)

        in resultValue : applyFunctions (tail functionList1) (tail functionList2) (tail valueList)
