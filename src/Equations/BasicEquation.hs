{-# LANGUAGE GADTs #-}

module Equations.BasicEquation (
      Equation(..)
    , eqMap
    , eqFst
    , eqSnd
) where

import Data.Bifunctor ( Bifunctor, bimap )

data Equation a b where
    (:~:) :: a -> b -> Equation a b 
    deriving (Eq)

instance (Show a, Show b) => Show (Equation a b) where 
    show (s :~: t) = show s ++ " = " ++ show t

instance Bifunctor Equation where
    bimap f g (s :~: t) = f s :~: g t

eqMap :: (a -> b) -> Equation a a -> Equation b b
eqMap f = bimap f f

eqFst :: Equation a b -> a
eqFst (x :~: y) = x

eqSnd :: Equation a b -> b
eqSnd (x :~: y) = y