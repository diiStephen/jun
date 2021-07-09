module Completion.CompletionUtils (
      TermOrder
    , orient
    , leftTerminatingOrient
    , rightTerminatingOrient
    , normalizeCriticalPair
    , mkEquation
) where

import Terms.Terms              ( Term(..) )
import Equations.BasicEquation  ( Equation(..) )
import TermRewriting.Rewrite    ( RewriteRule(..), RewriteSystem(..), normalize )
import Orders.PolyOrders        ( Order(..) )
import Confluence.CriticalPairs ( CriticalPair(..) )

import Control.Applicative ( (<|>) )

type TermOrder = Term -> Term -> Order

orient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
orient order eq = leftTerminatingOrient order eq <|> rightTerminatingOrient order eq

leftTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
leftTerminatingOrient order (s :~: t) = case order s t of 
    GR -> Just $ Rule s t 
    _ -> Nothing

rightTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule
rightTerminatingOrient order (s :~: t) = case order t s of 
    GR -> Just $ Rule t s
    _ -> Nothing

normalizeCriticalPair :: RewriteSystem -> CriticalPair -> CriticalPair
normalizeCriticalPair trs c = CP { left = normalize trs (left c), right = normalize trs (right c) }

mkEquation :: CriticalPair -> Equation Term Term
mkEquation c = left c :~: right c