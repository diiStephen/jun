module ForwardClosure.Closure (
      computeFowardClosure
    , forwardOverlap
    , forwardOverlaps 
    , fov
) where

import TermRewriting.Rewrite      ( RewriteSystem (..), RewriteRule (..), mkDisjointVars )
import Unification.Unification    ( unify' )
import Substitution.Substitutions ( applyLifted )
import Terms.Terms                ( get, set, label, isNonVar ) 
import Data.Maybe                 ( catMaybes ) 
import Data.List                  ( union )

data ForwardClosureEnv = Env {
      newRules :: RewriteSystem
    , killswitch :: Int 
}

computeFowardClosure :: RewriteSystem -> RewriteSystem 
computeFowardClosure = undefined 

-- Assume Var(rho1) \cap Var(rho2) = \varnothing 
forwardOverlap :: RewriteRule -> RewriteRule -> String -> Maybe RewriteRule 
forwardOverlap rho1 rho2 p = case unify' (get (rhs rho1) p) (lhs rho2) of 
    Just sigma -> Just $ Rule (applyLifted sigma (lhs rho1)) (set (rhs rho1) (rhs rho2) p)
    Nothing    -> Nothing

forwardOverlaps :: RewriteRule -> RewriteRule -> [Maybe RewriteRule]
forwardOverlaps rho1 rho2 = forwardOverlap (mkDisjointVars rho1 rho2) rho2 <$> nonVarPos
    where nonVarPos = snd <$> filter (isNonVar . fst) (label (rhs rho1))

fov :: RewriteSystem -> RewriteSystem -> [RewriteRule]
fov r1 r2 = catMaybes (foldr union [] [forwardOverlaps rho1 rho2 | rho1 <- rules r1, rho2 <- rules r2])