module Completion.HuetCompletion (
    complete
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..), orient )
import Terms.Terms                ( Term(..) )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), mkRewriteSystem, normalize, addRule )
import Equations.BasicEquation    ( Equation(..), eqMap, eqFst, eqSnd )
import Control.Monad              ( liftM )
import Control.Monad.RWS          ( RWST, gets, get, put, ask, tell )
import Control.Monad.Except       ( ExceptT, throwError )
import Control.Monad.Identity     ( Identity )
import Data.List                  ( union )
import Data.Bifunctor             ( second )
import Data.Maybe                 ( mapMaybe )

data Mark a 
    = Marked a 
    | Unmarked a
    deriving (Show, Eq)

instance Functor Mark where 
    fmap f (Marked x)   = Marked $ f x 
    fmap f (Unmarked x) = Unmarked $ f x

type MarkedRule = Mark RewriteRule 

data CompletionEnv = Env {
      eqs :: [Equation Term Term]
    , rules :: [(Int, MarkedRule)]
    , index :: Int 
}

type Log = [String] 

type CompletionM a = ExceptT CompletionFailure (RWST TermOrder Log CompletionEnv Identity) a

complete :: [Equation Term Term] -> Maybe RewriteSystem 
complete = undefined 

eval :: CompletionM RewriteSystem 
eval = do 
    (Env eqs rs i) <- get
    if not (null eqs)
    then infer 
    else undefined 
    undefined

-- Implements the inner loop of Huet's completion procedure. 
-- Current implementation feels "too imperative" with abusing monads. 
-- One iteration of the INNER while loop  
infer :: CompletionM ()
infer = do 
    (Env es rs i) <- get
    let e = head es -- Get the first equation. This is pretty dangerous.  
    let rewriteSystem = mkRewriteSystem $ map (fromMarked . snd) rs
    let enorm = eqMap (normalize rewriteSystem) e 
    if eqFst enorm == eqSnd enorm  
    then do
        tell ["[DELETE: " ++ show e ++ "]"]
        put $ Env (tail es) rs i
    else do 
        ord <- ask
        case orient ord enorm of
            Nothing -> do
                tell ["FAIL: Could not orient equation" ++ show enorm]
                throwError CFail
            Just r -> do
                tell ["[ORIENT: " ++ show r ++ "]"]
                put $ Env (tail es) rs i -- Remove the eq s = t that was just orientated 
                incIndex                 -- Increment the global index. 
                lSimplifyRewriteSystem r -- Update the equations first as new eqs are generated from R_{i} not R_{i+1}
                rSimplifyRewriteSystem r -- Generate rhs simplified rules part of R_{i+1} from R_{i} and r
                addNewRule r             -- Finally, add the new rule to the rewrite system to generate R_{i+1}

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs [] 0

rSimplifyRewriteSystem :: RewriteRule -> CompletionM () 
rSimplifyRewriteSystem rule = do 
    (Env eqs rs i) <- get
    let rewriteSystem = mkRewriteSystem $ map (fromMarked . snd) rs 
    let rsNew         = addRule rewriteSystem rule
    let reducedSystem = (map . second . fmap) (rSimplifyRule rsNew rule) rs 
    put $ Env eqs reducedSystem i

rSimplifyRule :: RewriteSystem -> RewriteRule -> RewriteRule -> RewriteRule
rSimplifyRule sys newRule oldRule | isIrreducible newRule (lhs oldRule) = Rule (lhs oldRule) (normalize sys (rhs oldRule))  
                                  | otherwise                           = oldRule

lSimplifyRewriteSystem :: RewriteRule -> CompletionM () 
lSimplifyRewriteSystem r = do
    (Env eqs rs i) <- get 
    let newEqs =  mapMaybe (\(_,rule) -> lSimplifyRule r (fromMarked rule)) rs --Reduce the LHS of the rules in R_{i} to generate new equations. 
    put $ Env (eqs ++ newEqs) rs i

lSimplifyRule :: RewriteRule -> RewriteRule -> Maybe (Equation Term Term)
lSimplifyRule newRule (Rule l r) | lNorm /= l = Just $ lNorm :~: r 
                                 | otherwise = Nothing
    where lNorm = normalize (mkRewriteSystem [newRule]) l

-- This should be a 1-liner 
incIndex :: CompletionM ()
incIndex = do
    (Env eqs rs i) <- get
    put $ Env eqs rs (i+1)

fromMarked :: Mark a -> a
fromMarked (Marked r) = r 
fromMarked (Unmarked r) = r 

isIrreducible :: RewriteRule -> Term -> Bool 
isIrreducible rule term = normalize (mkRewriteSystem [rule]) term == term

addNewRule :: RewriteRule -> CompletionM ()
addNewRule r = do 
    (Env eqs rs i) <- get 
    put $ Env eqs ((i, Unmarked r):rs) i