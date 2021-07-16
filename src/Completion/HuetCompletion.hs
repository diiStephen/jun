module Completion.HuetCompletion (
    complete
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..), orient )
import Terms.Terms                ( Term(..) )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), mkRewriteSystem, normalize )
import Equations.BasicEquation    ( Equation(..), eqMap, eqFst, eqSnd )
import Control.Monad              ( liftM )
import Control.Monad.RWS          ( RWST, gets, get, put, ask, tell )
import Control.Monad.Except       ( ExceptT, throwError )
import Control.Monad.Identity     ( Identity )
import Data.List                  ( union )

data Mark a 
    = Marked a 
    | Unmarked a
    deriving (Show, Eq)

type MarkedRule = Mark RewriteRule -- May not be necessary to have a type for this. 

data CompletionEnv = Env {
      eqs :: [Equation Term Term]
    , markedRs :: [(Int, RewriteRule)]
    , unMarkedRs :: [(Int, RewriteRule)] 
    , index :: Int 
}

type Log = [String] 

type CompletionM a = ExceptT CompletionFailure (RWST TermOrder Log CompletionEnv Identity) a

complete :: [Equation Term Term] -> Maybe RewriteSystem 
complete = undefined 

eval :: CompletionM RewriteSystem 
eval = undefined 

infer:: CompletionM ()
infer = do 
    es <- gets eqs
    mrs <- gets markedRs
    urs <- gets unMarkedRs
    let e = head es 
    let rs = mkRewriteSystem (map snd mrs ++ map snd urs)
    let enorm = eqMap (normalize rs) e 
    if eqFst enorm == eqSnd enorm  
    then do
        env <- get 
        put $ Env (tail es) (markedRs env) (unMarkedRs env) (index env)
    else do 
        ord <- ask
        case orient ord enorm of
            Nothing -> do
                tell ["FAIL: Could not orient equation" ++ show enorm]
                throwError CFail
            Just r -> do
                updateIndex
                updateRewriteSystem r
                updateEquations

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env eqs [] [] 0

updateRewriteSystem :: RewriteRule -> CompletionM () 
updateRewriteSystem = undefined 

updateEquations :: CompletionM () 
updateEquations = undefined 

updateIndex :: CompletionM ()
updateIndex = undefined