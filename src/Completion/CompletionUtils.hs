module Completion.CompletionUtils (
      orient
    , leftTerminatingOrient
    , rightTerminatingOrient
    , TermOrder
) where

import Terms.Terms             ( Term(..) )
import Equations.BasicEquation ( Equation(..) )
import TermRewriting.Rewrite   ( RewriteRule(..) )
import Orders.PolyOrders       ( Order(..) )

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