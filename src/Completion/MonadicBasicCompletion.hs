{-# LANGUAGE  FlexibleContexts #-}

module Completion.MonadicBasicCompletion (
      completionPhaseOne
    , runBasicCompletion
) where

import Terms.Terms                ( OrderedSig, Term(..) )
import Confluence.CriticalPairs   ( CriticalPair(..), allCriticalPairs )
import TermRewriting.Rewrite      ( RewriteSystem(..), RewriteRule(..), addRule )
import Equations.BasicEquation    ( Equation(..), eqFst, eqSnd )
import Orders.PolyOrders          ( Order(..) )
import Completion.BasicCompletion ( normalizeCriticalPair, mkEquation )

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

runBasicCompletion :: (Term -> Term -> Order) -> [Equation Term Term] -> IO RewriteSystem
runBasicCompletion order eqs = do
    let initialState = Env {comperator = order, criticalPairs=[], rewriteSystem=Rules [], termEquations=eqs} 
    let result = runIdentity (runStateT (runWriterT (runExceptT completionPhaseOne)) initialState)
    showTrace ((snd .fst) result)
    case (fst . fst) result of
        Right rs -> return rs
        Left CFail -> return (Rules [])
 
showTrace :: Foldable t => t String -> IO ()
showTrace = mapM_ putStrLn 

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
    mapM_ joinCriticalPair currCriticalPairs
    newRules <- gets rewriteSystem
    if length (rules newRules) == length (rules oldRules) 
        then 
            return newRules
        else completionPhaseTwo

joinCriticalPair ::CriticalPair -> CompletionEval ()
joinCriticalPair c = do
    trs <- gets rewriteSystem
    order <- gets comperator
    let normalizedC = normalizeCriticalPair trs c
    if left normalizedC == right normalizedC 
        then tell ["[Delete: " ++ show c ++ "]"] 
        else 
            do 
                case orient order (mkEquation normalizedC) of
                    Just rule -> do 
                        tell ["[Orient: " ++ show rule ++ "]"]
                        env <- get
                        put (setRules env (addRule (rewriteSystem env) rule ))
                    Nothing -> do 
                        tell ["Could not orient pair: " ++ show normalizedC]
                        throwError CFail

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