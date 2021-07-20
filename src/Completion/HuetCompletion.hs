{-# LANGUAGE TupleSections #-}

module Completion.HuetCompletion (
      CompletionEnv (..)
    , complete
    , choose
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..), orient, mkEquation )
import Terms.Terms                ( Term(..), size )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), mkRewriteSystem, normalize, addRule )
import Equations.BasicEquation    ( Equation(..), eqMap, eqFst, eqSnd )
import Confluence.CriticalPairs   ( allCriticalPairs, criticalPairs )
import Control.Monad              ( liftM )
import Control.Monad.RWS          ( RWS, gets, get, put, ask, tell, execRWS, runRWS )
import Control.Monad.Except       ( ExceptT, throwError, runExceptT, runExcept )
import Control.Monad.Identity     ( Identity )
import Data.List                  ( union )
import Data.Bifunctor             ( second )
import Data.Maybe                 ( mapMaybe, catMaybes )

data CompletionEnv = Env {
      eqs :: [Equation Term Term]
    , markedRules :: [(Int, RewriteRule)]
    , unmarkedRules :: [(Int, RewriteRule)]
    , index :: Int 
} deriving (Show)

type Log = [String] 

type CompletionM = ExceptT CompletionFailure (RWS TermOrder Log CompletionEnv) 

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs [] [] 0

complete :: [Equation Term Term]
 -> TermOrder 
 -> (Either CompletionFailure (), CompletionEnv, Log)
complete eqs order = runRWS (runExceptT eval) order (initCompletionEnv eqs)  

-- Implements the outer loop of Huet's procedure. 
eval :: CompletionM () 
eval = do
    (Env eqns markedRs unmarkedRs i) <- get
    case eqns of  
        (e:es) -> infer >> eval 
        []     -> case unmarkedRs of 
                       ((i,r):rs) -> do 
                           let (minUnmarkedRule, otherUnmarkedRules) = choose (map snd rs) r [] (size (lhs r) + size (rhs r)) 
                               newEqns = map mkEquation (catMaybes 
                                (concatMap (criticalPairs r . snd) markedRs
                                ++ concatMap (flip criticalPairs r . snd) markedRs
                                ++ criticalPairs r r))
                               indexOtherRules = map (i,) otherUnmarkedRules
                           put $ Env newEqns ((i,minUnmarkedRule):markedRs) indexOtherRules (i+1) --Indexing is wrong. 
                           eval
                       [] -> do 
                           tell ["Success!"]
                           return ()

-- Implements the inner loop of Huet's completion procedure. 
-- Current implementation feels "too imperative" with abusing monads. 
-- One iteration of the INNER while loop  
-- May be able to use the monad state modify function which will accept a function s -> s
infer :: CompletionM ()
infer = do 
    (Env eqns markedRs unmarkedRs i) <- get
    case eqns of 
        [] -> throwError CFail 
        (e:es) -> do
            let rewriteSystem = mkRewriteSystem $ map snd (markedRs ++ unmarkedRs)
                enorm         = eqMap (normalize rewriteSystem) e 
            if eqFst enorm == eqSnd enorm  
                then do
                    tell ["[DELETE: " ++ show e ++ "]"]
                    put $ Env es markedRs unmarkedRs i
                else do 
                    ord <- ask
                    case orient ord enorm of
                        Nothing -> do
                            tell ["FAIL: Could not orient equation " ++ show enorm]
                            throwError CFail
                        Just r -> do
                            tell ["[ORIENT: " ++ show r ++ "]"]
                            put $ Env es markedRs unmarkedRs i -- Remove the eq s = t that was just orientated 
                            incIndex                           -- Increment the global index. 
                            lSimplifyRewriteSystem r           -- Update the equations first as new eqs are generated from R_{i} not R_{i+1}
                            rSimplifyRewriteSystem r           -- Generate rhs simplified rules part of R_{i+1} from R_{i} and r
                            addNewRule r                       -- Finally, add the new rule to the rewrite system to generate R_{i+1}

rSimplifyRewriteSystem :: RewriteRule -> CompletionM () 
rSimplifyRewriteSystem rule = do 
    (Env eqs markedRs unmarkedRs i) <- get
    let rewriteSystem     = mkRewriteSystem $ map snd (markedRs ++ unmarkedRs) 
        rsNew             = addRule rewriteSystem rule
        reducedMarkedRs   = (map . second) (rSimplifyRule rsNew rule) markedRs
        reducedUnmarkedRs = (map . second) (rSimplifyRule rsNew rule) unmarkedRs
    put $ Env eqs reducedMarkedRs reducedUnmarkedRs i

rSimplifyRule :: RewriteSystem -> RewriteRule -> RewriteRule -> RewriteRule
rSimplifyRule sys newRule oldRule | isIrreducible newRule (lhs oldRule) = Rule (lhs oldRule) (normalize sys (rhs oldRule))  
                                  | otherwise                           = oldRule

isIrreducible :: RewriteRule -> Term -> Bool 
isIrreducible rule term = normalize (mkRewriteSystem [rule]) term == term

lSimplifyRewriteSystem :: RewriteRule -> CompletionM () 
lSimplifyRewriteSystem r = do
    (Env eqs markedRs unmarkedRs i) <- get 
    let newEqs =  mapMaybe (\(_,rule) -> lSimplifyRule r rule) (markedRs ++ unmarkedRs) --Reduce the LHS of the rules in R_{i} to generate new equations. 
    put $ Env (eqs ++ newEqs) markedRs unmarkedRs i

lSimplifyRule :: RewriteRule -> RewriteRule -> Maybe (Equation Term Term)
lSimplifyRule newRule (Rule l r) | lNorm /= l = Just $ lNorm :~: r 
                                 | otherwise = Nothing
    where lNorm = normalize (mkRewriteSystem [newRule]) l

incIndex :: CompletionM ()
incIndex = do
    (Env eqs markedRs unmarkedRs i) <- get
    put $ Env eqs markedRs unmarkedRs (i+1)

addNewRule :: RewriteRule -> CompletionM ()
addNewRule r = do 
    (Env eqs markedRs unmarkedRs i) <- get 
    put $ Env eqs markedRs ((i, r):unmarkedRs) i

--Currently need to split the rules so that the first test is not in the list of other rules. 
choose :: [RewriteRule] 
 -> RewriteRule 
 -> [RewriteRule] 
 -> Int 
 -> (RewriteRule, [RewriteRule])
choose [] currMinRule otherRules _ = (currMinRule, otherRules)
choose (r:rs) currMinRule otherRules currMinSize = if currSize < currMinSize 
    then choose rs r (currMinRule:otherRules) currSize  
    else choose rs currMinRule (r:otherRules) currMinSize  
    where 
        currSize = size (lhs r) + size (rhs r)