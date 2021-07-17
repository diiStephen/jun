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
eval = undefined 

-- Implements the inner loop of Huet's completion procedure. 
infer:: CompletionM ()
infer = do 
    (Env es rs i) <- get
    let e = head es -- Get the first equation 
    let rewriteSystem = mkRewriteSystem $ map (fromMarked . snd) rs
    let enorm = eqMap (normalize rewriteSystem) e 
    if eqFst enorm == eqSnd enorm  
    then do
        tell ["DELETE: " ++ show e]
        env <- get 
        put $ Env (tail es) rs i
    else do 
        ord <- ask
        case orient ord enorm of
            Nothing -> do
                tell ["FAIL: Could not orient equation" ++ show enorm]
                throwError CFail
            Just r -> do
                incIndex
                updateRewriteSystem r
                updateEquations r

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs [] 0

updateRewriteSystem :: RewriteRule -> CompletionM () 
updateRewriteSystem rule = do 
    (Env eqs rs i) <- get
    let rewriteSystem = mkRewriteSystem $ map (fromMarked . snd) rs 
    let rsNew         = addRule rewriteSystem rule
    let reducedSystem = (map . second . fmap) (rSimplifyRule rsNew rule) rs 
    put $ Env eqs reducedSystem i

rSimplifyRule :: RewriteSystem -> RewriteRule -> RewriteRule -> RewriteRule
rSimplifyRule sys newRule oldRule | isIrreducible newRule (lhs oldRule) = Rule (lhs oldRule) (normalize sys (rhs oldRule))  
                                  | otherwise                           = oldRule

updateEquations :: RewriteRule -> CompletionM () 
updateEquations r = do
    (Env eqs rs i) <- get 
    let newEqs = (map . second . fmap) (lSimplifyRule r) rs
    undefined

lSimplifyRule :: RewriteRule -> RewriteRule -> Equation Term Term
lSimplifyRule newRule (Rule l r) = lNorm :~: r
    where lNorm = normalize (mkRewriteSystem [newRule]) l

-- This should be a 1-liner 
incIndex :: CompletionM ()
incIndex = do
    (Env eqs rs i) <- get
    put $ Env eqs rs (i+1)

fromMarked :: MarkedRule -> RewriteRule
fromMarked (Marked r) = r 
fromMarked (Unmarked r) = r 

isIrreducible :: RewriteRule -> Term -> Bool 
isIrreducible rule term = normalize (mkRewriteSystem [rule]) term == term