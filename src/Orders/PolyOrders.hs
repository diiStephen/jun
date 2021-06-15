{-# LANGUAGE ScopedTypeVariables #-}

module Orders.PolyOrders (
    Order(..),
    lexOrd,
    multiOrder
) where

import Data.List(any, all)

data Order 
    = GR 
    | E 
    | NGE 
    deriving (Show, Eq)

lexOrd :: (a -> a -> Order) -> [a] -> [a] -> Order 
lexOrd _ [] []         = E 
lexOrd order (x:xs) (y:ys) = case order x y of 
    GR  -> GR 
    E   -> lexOrd order xs ys  
    NGE -> NGE

-- M >_mul N <=> M \not = N /\ \forall n \in N - M. \exists m \in M - N . m > n 
multiOrder :: forall a . (a -> a -> Order) -> [a] -> [a] -> Order 
multiOrder order ms ns = if (null nMinusM) && (null mMinusN) 
    then E else verify order nMinusM mMinusN 
    where 
        nMinusM = multiSetMinus order ns ms 
        mMinusN = multiSetMinus order ms ns
        verify :: (a -> a -> Order) -> [a] -> [a] -> Order 
        verify order u v = if all (\n -> any (\m -> (order m n) == GR) mMinusN) nMinusM 
            then GR else NGE 

multiSetMinus :: (a -> a -> Order) -> [a] -> [a] -> [a]
multiSetMinus _ xs []     = xs 
multiSetMinus order xs (y:ys) = multiSetMinus order (dropOne order xs y) ys 

dropOne :: (a -> a -> Order) -> [a] -> a -> [a]
dropOne _ [] _     = [] 
dropOne order (x:xs) y = case order x y of 
    E -> xs 
    _ -> x : (dropOne order xs y)
