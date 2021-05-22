module Orders.PolyOrders where

data Order = GR | E | NGE deriving (Show)

class Orderable a where 
    order :: a -> a -> Order 

lexOrd :: Orderable a => [a] -> [a] -> Order 
lexOrd [] [] = E 
lexOrd (x:xs) (y:ys) = case order x y of 
    GR  -> GR 
    E   -> lexOrd xs ys  
    NGE -> NGE

multiOrder :: Orderable a => [a] -> [a] -> Order 
multiOrder = undefined 