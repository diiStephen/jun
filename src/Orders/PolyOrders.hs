module Orders.PolyOrders where

import Data.List(find)

data Order = GR | E | NGE deriving (Show, Eq)

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

-- M >_mul N <=> M \not = N /\ \forall n \in N - M. \exists m \in M - N . m > n 
multiOrder :: Orderable a => [a] -> [a] -> Order 
multiOrder ms ns = if (null x) && (null y) then E else verify x y 
    where 
        x = multiSetMinus ns ms 
        y = multiSetMinus ms ns 
        verify [] v     = GR
        verify (u:us) v = case find (\p -> (order p u) == GR) v of 
            Just t -> verify us v 
            Nothing -> NGE  

multiSetMinus :: Orderable a => [a] -> [a] -> [a]
multiSetMinus xs []     = xs 
multiSetMinus xs (y:ys) = multiSetMinus (dropOne xs y) ys 

dropOne :: Orderable a => [a] -> a -> [a]
dropOne [] _     = [] 
dropOne (x:xs) y = case order x y of 
    E -> xs 
    _ -> x : (dropOne xs y)
