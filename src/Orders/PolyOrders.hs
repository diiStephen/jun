module Orders.PolyOrders where

data Order = GR | E | NGE deriving (Show)

class Orderable a where 
    order :: a -> a -> Order 

instance Orderable Int where 
    order x y | x == y = E 
              | x < y  = NGE 
              | x > y  = GR

lexOrd :: Orderable a => [a] -> [a] -> Order 
lexOrd [] [] = E 
lexOrd (x:xs) (y:ys) = case order x y of 
    GR  -> GR 
    E   -> lexOrd xs ys  
    NGE -> NGE

multiOrder :: Orderable a => [a] -> [a] -> Order 
multiOrder ms ns = undefined  

multiSetMinus :: Orderable a => [a] -> [a] -> [a]
multiSetMinus xs []     = xs 
multiSetMinus xs (y:ys) = multiSetMinus (dropOne xs y) ys 

dropOne :: Orderable a => [a] -> a -> [a]
dropOne [] _     = [] 
dropOne (x:xs) y = case order x y of 
    E -> xs 
    _ -> x : (dropOne xs y)
