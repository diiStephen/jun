module Unification (
    unify,
    unify',
    match,
    match'  
) where 

import Terms
import Substitutions

match' :: Term -> Term -> Maybe Subst 
match' s t = match [(s,t)] [] 

match :: [(Term, Term)] -> Subst -> Maybe Subst 
match [] s = Just s 
match (((V x), t):eq) subst = 
    if indom subst x then 
        if apply subst x == t then match eq subst 
        else Nothing 
    else match eq ([(x,t)]++subst)  

match ((t, (V x)):eq) subst = Nothing  

match (((T f ts), (T g ss)):eq) subst = 
    if f == g then match (decompose ((T f ts), (T g ss)) eq) subst 
    else Nothing 

unify' :: Term -> Term -> Maybe Subst 
unify' s t = unify [(s,t)] [] 

unify :: [(Term, Term)] -> Subst -> Maybe Subst 
unify [] s = Just s
unify ( ( (T f ts), (T g ss) ):eq ) subst 
    | f == g = if ts == ss 
        then unify eq subst 
        else unify (decompose ((T f ts), (T g ss)) eq) subst 
    | otherwise = Nothing 

unify (((V x),t):eq) subst 
    | V x == t = unify eq subst 
    | otherwise = (elim x t eq subst) 

unify ((t,(V x)):eq) subst = unify (orient (t, V x) eq) subst 

decompose :: (Term,Term) -> [(Term,Term)] -> [(Term,Term)]
decompose ((T _ ts), (T _ ss)) eq = (zip ts ss) ++ eq 

orient :: (Term,Term) -> [(Term,Term)] -> [(Term,Term)]
orient (s,t) eq = (t,s) : eq

elim :: VName -> Term -> [(Term,Term)] -> Subst -> Maybe Subst 
elim x t eq subst = 
    if occurs x t then Nothing 
    else 
        let xt = applyLifted [(x,t)] 
        in unify (map (\(t1,t2) -> (xt t1, xt t2)) eq)  ((x,t) : subst)