module ForwardClosure.Closure (
      computeFowardClosure
    , forwardOverlap
) where

import TermRewriting.Rewrite      ( RewriteSystem, RewriteRule (..) )
import Unification.Unification    ( unify' )
import Substitution.Substitutions ( applyLifted )
import Terms.Terms                ( get, set ) 

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