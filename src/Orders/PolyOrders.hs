module Orders.PolyOrders where

data EEQ = ELT | EEQ | EGT deriving (Show) 

lexOrd :: Ord a => [a] -> [a] -> EEQ
lexOrd [] [] = EEQ
lexOrd [] y  = ELT
lexOrd x []  = EGT  
lexOrd (x:xs) (y:ys) | x < y     = ELT  
                     | x > y     = EGT  
                     | otherwise = lexOrd xs ys