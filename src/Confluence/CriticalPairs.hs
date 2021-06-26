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


data CriticalPair = CP { left :: Term, right :: Term }

instance Show CriticalPair where 
    show (CP l r) = "〈 " ++ show l ++ " , " ++ show r ++ " 〉"

type CriticalPairs = [CriticalPair]

type Eval a = StateT CriticalPairs Identity a

criticalPair :: RewriteRule -> RewriteRule -> String -> Maybe CriticalPair
criticalPair r1 r2 p = case unify' (get (lhs r1) p) (lhs r2) of
    Just sigma -> Just (CP (applyLifted sigma (rhs r1)) (set (applyLifted sigma (lhs r1)) (applyLifted sigma (rhs r2)) p))
    Nothing    -> Nothing

criticalPairs :: RewriteRule -> RewriteRule -> [Maybe CriticalPair]
criticalPairs rho tau = criticalPair (mkDisjointVars rho tau) tau <$> nonVarPos
    where nonVarPos = snd <$> filter (\(t,_) -> isNonVar t) (label (lhs rho))

allCriticalPairs :: RewriteSystem -> [[Maybe CriticalPair]]
allCriticalPairs trs =  [criticalPairs rho tau | rho <- rules trs, tau <- rules trs]