module Orders.PolyOrders where

data Order = GR | E | NGE deriving (Show)

lexOrd :: (a -> b -> Order) -> [a] -> [b] -> Order 
lexOrd _ [] [] = E 
lexOrd order (x:xs) (y:ys) = case order x y of 
    GR -> GR 
    E -> lexOrd order xs ys  
    NGE -> NGE  