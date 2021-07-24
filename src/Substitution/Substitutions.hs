module Substitution.Substitutions (
    Subst,
    apply,
    applyLifted,
    indom
) where 

import Terms.Terms

type Subst = [(VName, Term)]

apply :: Subst -> VName -> Term 
apply [] y = V y 
apply ((x, t):ss) y = if x == y then t else apply ss y

applyLifted :: Subst -> Term -> Term 
applyLifted s (V x) = if (indom s x) then apply s x else (V x)
applyLifted s (T f ts) = T f (map (applyLifted s) ts) 

indom :: Subst -> VName -> Bool
indom [] v = False 
indom (s:ss) v = if fst s == v then True else indom ss v

