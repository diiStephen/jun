module Confluence.ConfluenceChecker (
    evalConfluence,
    checkConfluence
) where 

import TermRewriting.Rewrite    ( RewriteSystem(..), RewriteRule(..), normalize )
import Confluence.CriticalPairs ( CriticalPair(..), allCriticalPairs, criticalPairs )
import Control.Monad.Identity   ( Identity (runIdentity) )
import Control.Monad.Reader     ( ReaderT, runReaderT, ask )
import Control.Monad.Writer     ( WriterT (runWriterT), tell )

type ConfluenceEval a = ReaderT RewriteSystem (WriterT [String] Identity) a

confluent :: ConfluenceEval Bool
confluent = do
    sys <- ask
    let criticalPairs = allCriticalPairs sys
    tell ["Generated " ++ show (length criticalPairs) ++ " critical pairs" ]
    result <- mapM joinable criticalPairs
    return (and result)

joinable :: CriticalPair -> ConfluenceEval Bool 
joinable c = do 
    tell ["Checking for joinability of critical pair " ++ show c]
    trs <- ask
    let basic = map (\r -> (lhs r, rhs r)) (rules trs) --Refactor rewriting module! 
    let uNormalForm = normalize basic (left c)
    let vNormalForm = normalize basic (right c)
    if uNormalForm == vNormalForm 
    then do
        tell ["Critical pair " ++ show c ++ " is joinable with normal form: " ++ show uNormalForm]
        return True 
    else do
        tell ["Critical pair " ++ show c ++ " cannot be joined."]
        tell ["Distinct normal forms: " ++ show uNormalForm ++ " and " ++ show vNormalForm ]
        return False

evalConfluence :: RewriteSystem -> (Bool, [String])
evalConfluence trs = runIdentity(runWriterT(runReaderT confluent trs))

checkConfluence :: RewriteSystem -> IO ()
checkConfluence trs = do
    putStrLn $ "Checking confluence of " ++ show trs
    let (result, log) = evalConfluence trs
    putStrLn $ "System is confluent: " ++ show result
    mapM_ putStrLn log
