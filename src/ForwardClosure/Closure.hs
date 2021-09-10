module ForwardClosure.Closure (
      computeFowardClosure
    , forwardOverlap
    , forwardOverlaps
    , fov
    , isStrictlyRedundant
    , isInstance
    , isInstanceSystem
) where

import TermRewriting.Rewrite      ( RewriteSystem (..), RewriteRule (..), mkDisjointVars, rewriteAll )
import Unification.Unification    ( unify', match' )
import Substitution.Substitutions ( applyLifted )
import Terms.Terms                ( get, set, label, isNonVar )
import Data.Maybe                 ( catMaybes )
import Data.List                  ( union, delete )

computeFowardClosure :: Int -> RewriteSystem -> RewriteSystem
computeFowardClosure limit rs = Rules $ foldr (union . fc (rules rs)) [] range
    where range = take limit [1..]

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
isRedundant rho r = isInstanceSystem rho r || isStrictlyRedundant rho r

-- l -> r is strickly redundant in R where l is reducible 
-- and r is a normal form of l iff a proper subterm of l is reducible  
isStrictlyRedundant :: RewriteRule -> [RewriteRule] -> Bool
isStrictlyRedundant rho rs = go properSubtermPos
    where properSubtermPos = delete "" (snd <$> filter (isNonVar . fst) (label (lhs rho)))
          go [] = False
          go (p:ps) = case rewriteAll rs (get (lhs rho) p) of
              Just t -> True
              Nothing -> go ps

isInstanceSystem :: RewriteRule -> [RewriteRule] -> Bool
isInstanceSystem _ [] = False
isInstanceSystem rho (r:rs) = isInstance rho r || isInstanceSystem rho rs

isInstance :: RewriteRule -> RewriteRule -> Bool
isInstance rho1 rho2 = let matches = (match' (lhs rho1) (lhs rho2), match' (rhs rho1) (rhs rho2)) in
    case matches of
        (Just _, Just _) -> True
        _ -> False

-- Forward closure
fc :: [RewriteRule] -> Int -> [RewriteRule]
fc r 0 = r
fc r k = fc r (k-1) `union` nr k r

-- New rules 
nr :: Int -> [RewriteRule] -> [RewriteRule]
nr 0 r = r
nr k r = n (nr (k-1) r) r (fc r (k-1))