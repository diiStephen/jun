{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Completion.HuetCompletion (
      CompletionEnv (..)
    , complete
    , extract
) where 

import Completion.CompletionUtils ( TermOrder, CompletionFailure(..), orient, mkEquation )
import Terms.Terms                ( Term(..), size )
import TermRewriting.Rewrite      ( RewriteRule(..), RewriteSystem(..), mkRewriteSystem, normalize, addRule )
import Equations.BasicEquation    ( Equation(..), eqMap, eqFst, eqSnd )
import Confluence.CriticalPairs   ( criticalPairs )
import Utils.MonadUtils           ( mapMaybeM )
import Control.Monad.RWS          ( RWS, gets, get, put, ask, tell, runRWS, modify )
import Control.Monad.Except       ( ExceptT, throwError, runExceptT )
import Data.Maybe                 ( catMaybes )

data CompletionEnv = Env {
      eqs :: [(Int,Equation Term Term)] 
    , markedRules :: [(Int, RewriteRule)]
    , unmarkedRules :: [(Int, RewriteRule)]
    , index :: Int 
} deriving (Show)

type Log = [String] 

type CompletionM = ExceptT CompletionFailure (RWS TermOrder Log CompletionEnv)

initCompletionEnv :: [Equation Term Term] -> CompletionEnv 
initCompletionEnv eqs = Env (indexInitEqs eqs) [] [] (length eqs)

indexInitEqs :: [Equation Term Term] -> [(Int, Equation Term Term)]
indexInitEqs es = zip [1..(length es)] es

complete :: [Equation Term Term]
 -> TermOrder 
 -> (Either CompletionFailure (), CompletionEnv, Log)
complete eqs order = runRWS (runExceptT eval) order (initCompletionEnv eqs)  

extract :: [Equation Term Term] 
 -> TermOrder 
 -> Maybe RewriteSystem 
extract eqs order = case result of 
    Left CFail -> Nothing 
    Right _ -> Just $ mkRewriteSystem $ map snd (markedRules env)
    where (result, env, _) = complete eqs order

-- Implements the outer loop of Huet's procedure. 
eval :: CompletionM () 
eval = do
    (Env eqns markedRs unmarkedRs _) <- get
    case eqns of  
        (_:_) -> infer >> eval 
        []     -> case unmarkedRs of 
                       ((j,r):rs) -> do 
                           let ((k,minUnmarkedRule), otherUnmarkedRules) = choose rs (j,r) [] (size (lhs r) + size (rhs r)) 
                           indexNewEqs <- overlapM k minUnmarkedRule markedRs
                           newIndex <- gets index
                           put $ Env indexNewEqs ((k,minUnmarkedRule):markedRs) otherUnmarkedRules (newIndex+1)
                           eval
                       [] -> do 
                           tell ["Your system is convergent."]
                           return ()

overlapM :: Int -> RewriteRule -> [(Int, RewriteRule)] -> CompletionM [(Int, Equation Term Term)]
overlapM unmarkedRuleIndex selectedUnmarkedRule markedRules = do 
    newEqsOne <- mapM (uncurry (criticalPairsM unmarkedRuleIndex selectedUnmarkedRule)) markedRules
    newEqsTwo <- mapM (\(markedIndex,markedRule) -> criticalPairsM markedIndex markedRule unmarkedRuleIndex selectedUnmarkedRule) markedRules
    newEqsThree <- criticalPairsM unmarkedRuleIndex selectedUnmarkedRule unmarkedRuleIndex selectedUnmarkedRule
    return $ concat (newEqsOne ++ newEqsTwo ++ [newEqsThree])

criticalPairsM :: Int
 -> RewriteRule
 -> Int
 -> RewriteRule
 -> CompletionM [(Int, Equation Term Term)]
criticalPairsM sourceIndex source targetIndex target = do
    let newEqs = map mkEquation $ catMaybes (criticalPairs source target)
    mapM_ (logOverlapM sourceIndex targetIndex) newEqs
    startIndex <- gets index 
    finalIndex <- gets ((+) (length newEqs) . index)
    modify $ \env -> env { index = finalIndex }
    return $ zip [startIndex..finalIndex] newEqs
    
-- Implements one iteration of the inner loop of Huet's completion procedure. 
infer :: CompletionM ()
infer = do 
    (Env eqns markedRs unmarkedRs i) <- get
    case eqns of 
        [] -> throwError CFail 
        ((k,e):es) -> do
            let rewriteSystem = mkRewriteSystem $ map snd (markedRs ++ unmarkedRs)
                enorm         = eqMap (normalize rewriteSystem) e 
            if eqFst enorm == eqSnd enorm  
                then do
                    logDeleteM k e
                    put $ Env es markedRs unmarkedRs i
                else do 
                    ord <- ask
                    case orient ord enorm of
                        Nothing -> do
                            tell ["FAIL: Could not orient: " ++ show enorm]
                            throwError CFail
                        Just r -> do
                            logOrientM k r
                            put $ Env es markedRs unmarkedRs i -- Remove the eq s = t that was just orientated 
                            incIndex                           -- Increment the global index. 
                            newRuleIndex <- gets index
                            lSimplifyRewriteSystem r newRuleIndex -- Update the equations first as new eqs are generated from R_{i} not R_{i+1}
                            rSimplifyRewriteSystem r newRuleIndex -- Generate rhs simplified rules part of R_{i+1} from R_{i} and r
                            addNewRule r newRuleIndex             -- Finally, add the new rule to the rewrite system to generate R_{i+1}

rSimplifyRewriteSystem :: RewriteRule -> Int -> CompletionM () 
rSimplifyRewriteSystem rule k = do 
    (Env eqs markedRs unmarkedRs _) <- get
    let rsNew = addRule (mkRewriteSystem $ map snd (markedRs ++ unmarkedRs)) rule 
    reducedMarkedRs <- mapMaybeM (uncurry (rSimplifyRuleM rsNew k rule)) markedRs
    reducedUnmarkedRs <- mapMaybeM (uncurry (rSimplifyRuleM rsNew k rule)) unmarkedRs
    updatedIndex <- gets index
    put $ Env eqs reducedMarkedRs reducedUnmarkedRs updatedIndex

rSimplifyRuleM :: RewriteSystem
 -> Int
 -> RewriteRule
 -> Int
 -> RewriteRule
 -> CompletionM (Maybe (Int, RewriteRule))
rSimplifyRuleM augRS reducerIndex reducerRule targetIndex targetRule = incIndex >>
    case result of 
        Just targetReduced -> logRewriteM reducerIndex targetIndex targetReduced >> pure (Just (targetIndex, targetReduced))
        Nothing -> pure Nothing
    where result = rSimplifyRule augRS reducerRule targetRule

rSimplifyRule :: RewriteSystem 
 -> RewriteRule 
 -> RewriteRule 
 -> Maybe RewriteRule
rSimplifyRule sys newRule oldRule | isIrreducible newRule (lhs oldRule) = Just $ Rule (lhs oldRule) (normalize sys (rhs oldRule))  
                                  | otherwise                           = Nothing

isIrreducible :: RewriteRule -> Term -> Bool 
isIrreducible rule term = normalize (mkRewriteSystem [rule]) term == term

lSimplifyRewriteSystem :: RewriteRule -> Int -> CompletionM () 
lSimplifyRewriteSystem r k = do
    (Env eqs markedRs unmarkedRs _) <- get 
    newEqs <- mapMaybeM (uncurry (lSimplifyRuleM k r)) (markedRs ++ unmarkedRs) --Reduce the LHS of the rules in R_{i} to generate new equations. 
    updatedIndex <- gets index
    put $ Env (eqs ++ newEqs) markedRs unmarkedRs updatedIndex

lSimplifyRuleM :: Int
 -> RewriteRule
 -> Int
 -> RewriteRule
 -> CompletionM (Maybe (Int, Equation Term Term))
lSimplifyRuleM reducerIndex reducerRule targetIndex targetRule = do 
    incIndex 
    currIndex <- gets index 
    case lSimplifyRule reducerRule targetRule of 
        Just eq -> logRewriteM reducerIndex targetIndex eq >> pure (Just (currIndex, eq))
        Nothing -> pure Nothing

lSimplifyRule :: RewriteRule -> RewriteRule -> Maybe (Equation Term Term)
lSimplifyRule newRule (Rule l r) | lNorm /= l = Just $ lNorm :~: r 
                                 | otherwise = Nothing
    where lNorm = normalize (mkRewriteSystem [newRule]) l

incIndex :: CompletionM ()
incIndex = modify $ \env -> env {index = index env + 1}

addNewRule :: RewriteRule -> Int -> CompletionM ()
addNewRule r k = modify $ \env -> env { unmarkedRules = (k,r):unmarkedRules env }
    
--Currently need to split the rules so that the first test is not in the list of other rules. 
choose :: [(Int,RewriteRule)] 
 -> (Int,RewriteRule) 
 -> [(Int, RewriteRule)] 
 -> Int 
 -> ((Int,RewriteRule), [(Int,RewriteRule)])
choose [] currMinRule otherRules _ = (currMinRule, otherRules)
choose ((i,r):rs) currMinRule otherRules currMinSize = if currSize < currMinSize 
    then choose rs (i,r) (currMinRule:otherRules) currSize  
    else choose rs currMinRule ((i,r):otherRules) currMinSize  
    where 
        currSize = size (lhs r) + size (rhs r)

logRewriteM :: (Show a) => Int -> Int -> a -> CompletionM ()
logRewriteM i j r = tell ["[REWRITE(" ++ show i ++ "," ++  show j ++ "): " ++ show r ++ "]"]

logOverlapM :: (Show a) => Int -> Int -> a -> CompletionM ()
logOverlapM sourceIndex targetIndex e = tell ["[OVERLAP(" ++ show sourceIndex ++ "," ++ show targetIndex ++ "): " ++ show e ++ "]"]

logDeleteM :: (Show a) => Int -> a -> CompletionM ()
logDeleteM indexDeleted deleted = tell ["[DELETE(" ++ show indexDeleted ++ "): " ++ show deleted ++ "]"]

logOrientM :: (Show a) => Int -> a -> CompletionM ()
logOrientM index rule = tell ["[ORIENT(" ++ show index ++ "): " ++ show rule ++ "]"]
