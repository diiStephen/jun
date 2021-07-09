{-# LANGUAGE GADTs, TypeOperators, DataKinds #-}

module Completion.BasicCompletion (
    complete,
    joinable,
    completionPhaseOne,
    completionPhaseTwo,
    completeCriticalPairs,
    mkEquation,
    normalizeCriticalPair
) where

import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), normalize, basic )
import Terms.Terms                ( OrderedSig, Term(..) )
import Confluence.CriticalPairs   ( CriticalPair(..), allCriticalPairs, criticalPairs )
import Orders.PolyOrders          ( Order(..) )
import Orders.TermOrders          ( lpo, mpo )
import Equations.BasicEquation    ( Equation(..) )
import Completion.CompletionUtils ( orient, TermOrder )

import Control.Applicative    ( (<|>) )

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
