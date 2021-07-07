{-# LANGUAGE GADTs, TypeOperators, DataKinds #-}

module Equations.BasicEquation (
      Equation(..)
    , eqMap
    , eqFst
    , eqSnd
) where

data Equation a b where
    (:~:) :: a -> b -> Equation a b 
    deriving (Eq)

instance (Show a, Show b) => Show (Equation a b) where 
    show (s :~: t) = show s ++ " = " ++ show t

eqMap :: (a -> b) -> Equation a a -> Equation b b
eqMap f (x :~: y) = f x :~: f y

eqFst :: Equation a b -> a
eqFst (x :~: y) = x

eqSnd :: Equation a b -> b
eqSnd (x :~: y) = y