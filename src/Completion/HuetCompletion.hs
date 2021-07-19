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

-- Seems like this type is just getting in the way rather than making things easier. 
-- It will be more efficient/readable to use two lists for marked/unmarked rules. 
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

eval :: CompletionM () 
eval = do 
    (Env eqs rs i) <- get
    if not (null eqs)
    then do
        infer
        eval
    else do
        case filter (isMarked . snd) rs of 
            [] -> return () --We're done. The system is in the environment.  
            (mr:mrs) -> do 
                let firstRule = (fromMarked . snd) mr
                    minMarkedRule = choose (map (fromMarked . snd) (mr:mrs)) firstRule (size (lhs firstRule) + size (rhs firstRule))
                undefined

-- Implements the inner loop of Huet's completion procedure. 
-- Current implementation feels "too imperative" with abusing monads. 
-- One iteration of the INNER while loop  
-- May be able to use the monad state modify function which will accept a function s -> s
infer :: CompletionM ()
infer = do 
    (Env es rs i) <- get
    let e             = head es -- TODO Change this to case analysis for [] and e:es 
        rewriteSystem = mkRewriteSystem $ map (fromMarked . snd) rs
        enorm         = eqMap (normalize rewriteSystem) e 
    if eqFst enorm == eqSnd enorm  
    then 
        do
            tell ["[DELETE: " ++ show e ++ "]"]
            put $ Env (tail es) rs i
    else 
        do 
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
        rsNew         = addRule rewriteSystem rule
        reducedSystem = (map . second . fmap) (rSimplifyRule rsNew rule) rs 
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

choose :: [RewriteRule] -> RewriteRule -> Int -> RewriteRule
choose [] currMin _ = currMin 
choose (r:rs) currMin s = if rSize < s 
    then choose rs r rSize
    else choose rs currMin s 
    where rSize = size (lhs r) + size (rhs r)

isMarked :: MarkedRule -> Bool 
isMarked r = case r of 
    Marked _ -> True 
    Unmarked _ -> False