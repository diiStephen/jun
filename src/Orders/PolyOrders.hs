module Orders.PolyOrders (
    Order(..),
    Orderable (order),
    lexOrd,
    multiOrder
) where

import Data.List(any, all)

data Order = GR | E | NGE deriving (Show, Eq)

class Orderable a where 
    order :: a -> a -> Order 

lexOrd :: (a -> a -> Order) -> [a] -> [a] -> Order 
lexOrd _ [] []         = E 
lexOrd order (x:xs) (y:ys) = case order x y of 
    GR  -> GR 
    E   -> lexOrd order xs ys  
    NGE -> NGE

-- M >_mul N <=> M \not = N /\ \forall n \in N - M. \exists m \in M - N . m > n 
multiOrder :: Orderable a => [a] -> [a] -> Order 
multiOrder ms ns = if (null nMinusM) && (null mMinusN) 
    then E else verify nMinusM mMinusN 
    where 
        nMinusM = multiSetMinus ns ms 
        mMinusN = multiSetMinus ms ns
        verify :: Orderable a => [a] -> [a] -> Order 
        verify u v = if all (\n -> any (\m -> (order m n) == GR) mMinusN) nMinusM 
            then GR else NGE 

multiSetMinus :: Orderable a => [a] -> [a] -> [a]
multiSetMinus xs []     = xs 
multiSetMinus xs (y:ys) = multiSetMinus (dropOne xs y) ys 

dropOne :: Orderable a => [a] -> a -> [a]
dropOne [] _     = [] 
dropOne (x:xs) y = case order x y of 
    E -> xs 
    _ -> x : (dropOne xs y)
