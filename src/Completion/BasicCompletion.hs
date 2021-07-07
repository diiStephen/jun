{-# LANGUAGE GADTs, TypeOperators, DataKinds #-}

module Completion.BasicCompletion (
    complete,
    leftOrient,
    rightOrient,
    orient,
    joinable,
    completionPhaseOne,
    completionPhaseTwo,
    completeCriticalPairs,
    mkEquation,
    normalizeCriticalPair
) where

import TermRewriting.Rewrite    ( RewriteRule(..), RewriteSystem(..), normalize, basic )
import Terms.Terms              ( OrderedSig, Term(..) )
import Confluence.CriticalPairs ( CriticalPair(..), allCriticalPairs, criticalPairs )
import Orders.PolyOrders        ( Order(..) )
import Orders.TermOrders        ( lpo, mpo )
import Equations.BasicEquation  ( Equation(..) )

import Control.Applicative    ( (<|>) )

type TermOrder = Term -> Term -> Order

completionPhaseOne :: TermOrder -> [Equation Term Term] -> RewriteSystem -> Maybe RewriteSystem
completionPhaseOne _ [] trs = Just trs 
completionPhaseOne order (e:eqs) trs = case orient order e of 
    Just rule -> completionPhaseOne order eqs (Rules $ rule:rules trs)  
    Nothing   -> Nothing

completeCriticalPairs :: TermOrder -> RewriteSystem -> [CriticalPair] -> Maybe RewriteSystem 
completeCriticalPairs _ trs [] = Just trs
completeCriticalPairs order trs (c:cs) = let nC = normalizeCriticalPair trs c in 
    if joinable nC then completeCriticalPairs order trs cs else 
        case orient order (mkEquation nC) of 
            Just rule -> completeCriticalPairs order (Rules $ rule:rules trs) cs
            Nothing   -> Nothing

completionPhaseTwo :: TermOrder -> RewriteSystem -> Maybe RewriteSystem 
completionPhaseTwo order trs = do 
    let rOld = trs 
    rComplete <- completeCriticalPairs order trs (allCriticalPairs trs) 
    if length (rules rComplete) == length (rules rOld) 
        then return rOld 
        else completionPhaseTwo order rComplete

complete :: TermOrder -> [Equation Term Term] -> Maybe RewriteSystem
complete order eqs = do 
    phaseOne <- completionPhaseOne order eqs (Rules [])
    completionPhaseTwo order phaseOne 

mkEquation :: CriticalPair -> Equation Term Term
mkEquation c = left c :~: right c

joinable :: CriticalPair -> Bool 
joinable cp = left cp == right cp 

normalizeCriticalPair :: RewriteSystem -> CriticalPair -> CriticalPair
normalizeCriticalPair trs c = CP { left = normalize (basic trs) (left c), right = normalize (basic trs) (right c) }

leftOrient :: Equation Term Term -> RewriteRule
leftOrient (x :~: y) = Rule x y

rightOrient :: Equation Term Term -> RewriteRule
rightOrient (x :~: y) = Rule y x 

leftTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
leftTerminatingOrient comp (s :~: t) = case comp s t of 
    GR -> Just (leftOrient (s :~: t))
    _  -> Nothing  

rightTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule
rightTerminatingOrient comp (s :~: t) = case comp t s of 
    GR -> Just (rightOrient (s :~: t))
    _  -> Nothing 

orient :: TermOrder -> Equation Term Term -> Maybe RewriteRule
orient comp eq = leftTerminatingOrient comp eq <|> rightTerminatingOrient comp eq


{-
Produced Just {f(i(f(i(x1),y3)),1) --> f(1,z3)} which is not even 
a rewrite rule as there is a variable that does not occur on the 
LHS. How did this get generated? It may be a bug with the normalize function 
or with the critical pairs function. 

The allCriticalPairs function produces weirdness for the following:
{
    f(i(f(x1,y1)),f(x1,f(y1,z1))) --> z1 , 
    f(i(1),x1) --> x1 , 
    f(x1,1) --> x1 , 
    f(i(i(x1)),z3) --> f(x1,z3) , 
    f(i(x1),f(x1,z3)) --> z3 ,
    f(i(x1),x1) --> 1 , 
    f(1,x1) --> x1 , 
    f(f(x1,y1),z1) --> f(x1,f(y1,z1))
}

The specific rules: 

p1 = f(i(f(x1,y1)),f(x1,f(y1,z1))) --> z1 , and 

p2 = f(i(x1),x1) --> 1

overlap at position 2 does it 

f(x3, f(y3, z3)) =? f(i(x1), x1)
x3 |-> i(x1)
x1 |-> f(y3, z3)

<z3, f(i(f(i(x1),y3)),1)> 

-}
