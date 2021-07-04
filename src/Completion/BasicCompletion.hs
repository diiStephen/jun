{-# LANGUAGE GADTs, TypeOperators, DataKinds #-}

module Completion.BasicCompletion (
    Equation(..),
    complete,
    eqMap,
    eqFst,
    eqSnd,
    leftOrient,
    rightOrient,
    orient
) where

import TermRewriting.Rewrite    ( RewriteRule(..), RewriteSystem(..) )
import Terms.Terms              ( OrderedSig, Term(..) )
import Confluence.CriticalPairs ( CriticalPair(..) )
import Orders.PolyOrders        ( Order(..) )
import Orders.TermOrders        ( lpo, mpo )

import Control.Applicative ( (<|>) )

data Equation a b where
    (:~:) :: a -> b -> Equation a b 

instance (Show a, Show b) => Show (Equation a b) where 
    show (s :~: t) = show s ++ " = " ++ show t

data CompletionEnvironment 
    = Env { 
        symbolOrdering :: OrderedSig, 
        criticalPairs :: [CriticalPair], 
        rewriteSystem :: RewriteSystem, 
        termEquations :: [Equation Term Term]
    } deriving (Show)

type TermOrder = Term -> Term -> Order

complete ::  [Equation Term Term] -> Maybe RewriteSystem
complete = undefined 

eqMap :: (a -> b) -> Equation a a -> Equation b b
eqMap f (x :~: y) = f x :~: f y

eqFst :: Equation a b -> a
eqFst (x :~: y) = x

eqSnd :: Equation a b -> b
eqSnd (x :~: y) = y

leftOrient :: Equation Term Term -> RewriteRule
leftOrient (x :~: y) = Rule x y

rightOrient :: Equation Term Term -> RewriteRule
rightOrient (x :~: y) = Rule y x 

leftTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
leftTerminatingOrient comp (s :~: t) = case comp s t of 
    GR -> return (leftOrient (s :~: t))
    _ -> Nothing  

rightTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule
rightTerminatingOrient comp (s :~: t) = case comp t s of 
    GR -> return (rightOrient (s :~: t))
    _ -> Nothing 

orient ::  TermOrder -> Equation Term Term -> Maybe RewriteRule
orient comp eq = leftTerminatingOrient comp eq <|> rightTerminatingOrient comp eq