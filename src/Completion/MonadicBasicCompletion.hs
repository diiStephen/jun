module Completion.MonadicBasicCompletion (
      completionPhaseOne
    , runBasicCompletion
) where

import Terms.Terms                ( OrderedSig, Term(..) )
import Confluence.CriticalPairs   ( CriticalPair(..), allCriticalPairs )
import TermRewriting.Rewrite      ( RewriteSystem(..), RewriteRule(..) )
import Equations.BasicEquation    ( Equation(..), eqFst, eqSnd )
import Orders.PolyOrders          ( Order(..) )
import Completion.BasicCompletion ( normalizeCriticalPair )

import Control.Monad.State    ( StateT (runStateT), gets, get, put, MonadState )
import Control.Monad.Writer   ( WriterT, tell, runWriterT )
import Control.Monad.Except   ( ExceptT, runExceptT, MonadError (throwError) )
import Control.Monad.Identity ( Identity, runIdentity )

import Control.Applicative ( (<|>) )
import Data.List           ( union )

type TermOrder = Term -> Term -> Order

data CompletionEnvironment 
    = Env {  
        comperator     :: TermOrder,
        criticalPairs  :: [CriticalPair], 
        rewriteSystem  :: RewriteSystem, 
        termEquations  :: [Equation Term Term]
    }

data CompletionFailure 
    = CFail
    deriving (Show)

type CompletionEval a = ExceptT CompletionFailure (WriterT [String] (StateT CompletionEnvironment Identity)) a

runBasicCompletion :: 
 TermOrder
 -> [Equation Term Term]
 -> (Either CompletionFailure RewriteSystem, [String])
runBasicCompletion order eqs =  
    let initialState = Env {comperator = order, criticalPairs=[], rewriteSystem=Rules [], termEquations=eqs} in
        fst $ runIdentity (runStateT (runWriterT (runExceptT completionPhaseOne)) initialState)
 
completionPhaseOne :: CompletionEval RewriteSystem
completionPhaseOne = do
    reducOrder <- gets comperator
    eqs <- gets termEquations
    let (rules, failed) = foldr (partitionOrientable reducOrder) ([],[]) eqs
    if null failed 
        then do 
            let trs = Rules rules
            env <- get
            put (setRules env trs)
            completionPhaseTwo
        else do 
            tell ["Could not orient equations: " ++ show failed ]
            throwError CFail 

completionPhaseTwo :: CompletionEval RewriteSystem
completionPhaseTwo = do
    currCriticalPairs <- gets (allCriticalPairs . rewriteSystem)
    oldRules <- gets rewriteSystem
    if null currCriticalPairs 
        then gets rewriteSystem
        else do
            throwError CFail

joinCriticalPair :: TermOrder -> RewriteSystem -> CriticalPair -> RewriteSystem
joinCriticalPair order trs c = undefined

setRules :: CompletionEnvironment -> RewriteSystem -> CompletionEnvironment
setRules env rs = Env (comperator env) (criticalPairs env) rs (termEquations env)

setCriticalPairs :: CompletionEnvironment -> [CriticalPair] -> CompletionEnvironment
setCriticalPairs env cs = Env (comperator env) (cs `union` criticalPairs env) (rewriteSystem env) (termEquations env)

partitionOrientable :: 
 TermOrder 
 -> Equation Term Term 
 -> ([RewriteRule], [Equation Term Term]) 
 -> ([RewriteRule], [Equation Term Term]) 
partitionOrientable order eq (rules, failed) = case orient order eq of 
    Just rule -> (rule:rules, failed)
    Nothing -> (rules, eq:failed)

orient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
orient order eq = leftTerminatingOrient order eq <|> rightTerminatingOrient order eq

leftTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule 
leftTerminatingOrient order (s :~: t) = case order s t of 
    GR -> Just $ Rule s t 
    _ -> Nothing

rightTerminatingOrient :: TermOrder -> Equation Term Term -> Maybe RewriteRule
rightTerminatingOrient order (s :~: t) = case order t s of 
    GR -> Just $ Rule t s
    _ -> Nothing