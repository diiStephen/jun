module Completion.HuetCompletion (
    complete
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..), orient )
import Terms.Terms                ( Term(..), size )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), mkRewriteSystem, normalize, addRule )
import Equations.BasicEquation    ( Equation(..), eqMap, eqFst, eqSnd )
import Control.Monad              ( liftM )
import Control.Monad.RWS          ( RWST, gets, get, put, ask, tell )
import Control.Monad.Except       ( ExceptT, throwError )
import Control.Monad.Identity     ( Identity )
import Data.List                  ( union )
import Data.Bifunctor             ( second )
import Data.Maybe                 ( mapMaybe )

data CompletionEnv = Env {
      eqs :: [Equation Term Term]
    , markedRules :: [(Int, RewriteRule)]
    , unmarkedRules :: [(Int, RewriteRule)]
    , index :: Int 
}

type Log = [String] 

type CompletionM a = ExceptT CompletionFailure (RWST TermOrder Log CompletionEnv Identity) a

complete :: [Equation Term Term] -> Maybe RewriteSystem 
complete = undefined 

eval :: CompletionM () 
eval = undefined

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
                            tell ["FAIL: Could not orient equation" ++ show enorm]
                            throwError CFail
                        Just r -> do
                            tell ["[ORIENT: " ++ show r ++ "]"]
                            put $ Env es markedRs unmarkedRs i -- Remove the eq s = t that was just orientated 
                            incIndex                           -- Increment the global index. 
                            lSimplifyRewriteSystem r           -- Update the equations first as new eqs are generated from R_{i} not R_{i+1}
                            rSimplifyRewriteSystem r           -- Generate rhs simplified rules part of R_{i+1} from R_{i} and r
                            addNewRule r                       -- Finally, add the new rule to the rewrite system to generate R_{i+1}

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs [] [] 0

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

isIrreducible :: RewriteRule -> Term -> Bool 
isIrreducible rule term = normalize (mkRewriteSystem [rule]) term == term

addNewRule :: RewriteRule -> CompletionM ()
addNewRule r = do 
    (Env eqs markedRs unmarkedRs i) <- get 
    put $ Env eqs markedRs ((i, r):unmarkedRs) i

choose :: [RewriteRule] -> RewriteRule -> Int -> RewriteRule
choose [] currMin _ = currMin 
choose (r:rs) currMin s = if rSize < s 
    then choose rs r rSize
    else choose rs currMin s 
    where rSize = size (lhs r) + size (rhs r)
