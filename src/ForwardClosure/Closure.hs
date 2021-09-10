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

-- FOV(R_1, R_2)
fov :: [RewriteRule] -> [RewriteRule] -> [RewriteRule]
fov r1 r2 = catMaybes (foldr union [] [forwardOverlaps rho1 rho2 | rho1 <- r1, rho2 <- r2])

-- N(R_1, R_2, R_3)
n :: [RewriteRule] -> [RewriteRule] -> [RewriteRule] -> [RewriteRule]
n r1 r2 r3 = [rho | rho <- fov r1 r2, not (isRedundant rho r3)]

isRedundant :: RewriteRule ->  [RewriteRule] -> Bool 
isRedundant rho r = isInstanceSystem rho r || isStricklyRedundant rho r  

isStricklyRedundant :: RewriteRule -> [RewriteRule] -> Bool 
isStricklyRedundant = undefined 

isInstanceSystem :: RewriteRule -> [RewriteRule] -> Bool 
isInstanceSystem = undefined

isInstance :: RewriteRule -> RewriteRule -> Bool 
isInstance = undefined

-- Forward closure
fc :: Int -> [RewriteRule] -> [RewriteRule] 
fc 0 r = r 
fc k r = fc (k-1) r `union` nr (k+1) r 

-- New rules 
nr :: Int -> [RewriteRule] -> [RewriteRule]
nr 0 r = r
nr k r = n (nr (k-1) r) r (fc (k+1) r)