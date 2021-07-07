module Completion.MonadicBasicCompletion (

) where

import Terms.Terms              ( OrderedSig, Term(..) )
import Confluence.CriticalPairs ( CriticalPair(..) )
import TermRewriting.Rewrite    ( RewriteSystem(..) )
import Equations.BasicEquation  ( Equation(..) )
import Orders.PolyOrders        ( Order(..) )

import Control.Monad.State    ( StateT, gets )

type TermOrder = Term -> Term -> Order

data CompletionEnvironment 
    = Env { 
        symbolOrdering :: OrderedSig, 
        comperator     :: TermOrder,
        criticalPairs  :: [CriticalPair], 
        rewriteSystem  :: RewriteSystem, 
        termEquations  :: [Equation Term Term]
    }

type CompletionEval a = StateT CompletionEnvironment Maybe a