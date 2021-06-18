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

instance Semigroup Order where 
    (<>) E x = x 
    (<>) GR _ = GR 
    (<>) NGE _ = NGE 

instance Monoid Order where 
    mempty = E

lexOrd :: (a -> a -> Order) -> [a] -> [a] -> Order 
lexOrd order xs ys = mconcat (zipWith order xs ys)

-- M >_mul N <=> M != N /\ \forall n \in N - M. \exists m \in M - N . m > n 
multiOrder :: forall a . (a -> a -> Order) -> [a] -> [a] -> Order 
multiOrder order ms ns | null nMinusM && null mMinusN = E  
                       | otherwise = verify order nMinusM mMinusN 
                       where 
                           nMinusM = multiSetMinus order ns ms 
                           mMinusN = multiSetMinus order ms ns
                           verify :: (a -> a -> Order) -> [a] -> [a] -> Order 
                           verify order u v = if all (\n -> any (\m -> order m n == GR) mMinusN) nMinusM 
                                              then GR else NGE 

multiSetMinus :: (a -> a -> Order) -> [a] -> [a] -> [a]
multiSetMinus _ xs []         = xs 
multiSetMinus order xs (y:ys) = multiSetMinus order (dropOne order xs y) ys 

dropOne :: (a -> a -> Order) -> [a] -> a -> [a]
dropOne _ [] _         = [] 
dropOne order (x:xs) y = case order x y of 
    E -> xs 
    _ -> x : dropOne order xs y
