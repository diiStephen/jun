module Confluence.CriticalPairs (
    CriticalPair (..),
    criticalPair,
    criticalPairs,
    allCriticalPairs
) where

import Terms.Terms                ( Term (..), alphaConvert, maxIndex, subterms, label, get, set, pos, label, isNonVar )
import TermRewriting.Rewrite      ( RewriteSystem (..), RewriteRule (..), mkDisjointVars )
import Unification.Unification    ( unify' )
import Substitution.Substitutions (Subst, applyLifted)
import Control.Monad.Identity     ( Identity )
import Control.Monad.State        ( StateT )
import Data.List                  ( union )    
import Data.Maybe                 (isJust, fromJust)               

data CriticalPair 
    = CP { left :: Term, right :: Term }
    deriving (Eq, Ord)

instance Show CriticalPair where 
    show (CP l r) = "〈 " ++ show l ++ " , " ++ show r ++ " 〉"

type CriticalPairs = [CriticalPair]

--l1_p ?= l2 
criticalPair :: RewriteRule -> RewriteRule -> String -> Maybe CriticalPair
criticalPair r1 r2 p = case unify' (get (lhs r1) p) (lhs r2) of
    Just sigma -> Just (CP (applyLifted sigma (rhs r1)) (applyLifted sigma (set (lhs r1) (rhs r2) p)))
    Nothing    -> Nothing

criticalPairs :: RewriteRule -> RewriteRule -> [Maybe CriticalPair]
criticalPairs rho tau = criticalPair (mkDisjointVars rho tau) tau <$> nonVarPos
    where nonVarPos = snd <$> filter (isNonVar . fst) (label (lhs rho))

allCriticalPairs :: RewriteSystem -> [CriticalPair]
allCriticalPairs trs = fromJust <$> filter isJust (foldr union [] [criticalPairs rho tau | rho <- rules trs, tau <- rules trs])