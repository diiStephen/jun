{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}

module Interface.Repl (
    repl
) where

import Terms.Terms                ( Term(..), OrderedSig, FSym )
import Terms.TermParser           ( getTerm )
import TermRewriting.Rewrite      ( RewriteSystem(..), RewriteRule(..), mkRewriteSystem )
import Equations.BasicEquation    ( Equation(..), eqMap )
import Completion.HuetCompletion  ( CompletionEnv(..), complete)
import Completion.CompletionUtils ( TermOrder )
import Orders.RecursivePathOrders ( lpo, mpo )
import Orders.KnuthBendixOrder    ( kbo )
import Control.Monad              ( void )
import Control.Monad.RWS          ( RWST, liftIO, gets, runRWST, modify, get )
import System.IO                  ( hFlush, stdout ) 

type Log = [String]

type ReplM a = (RWST String Log ReplEnv) IO a

data ReplEnv = REnv {
      signature :: OrderedSig
    , curRewriteSystem :: RewriteSystem
    , currRules :: [RewriteRule] 
    , curEquations :: [Equation Term Term] 
} deriving (Show)

defaultPrompt :: ReplEnv -> String
defaultPrompt env = show (signature env) 
    ++ "[E " ++ show (length $ curEquations env) ++ "]" 
    ++ "[R " ++ show (length $ rules $ curRewriteSystem env) ++ "]" 
    ++ ":> "

commands :: [String]
commands = ["[exit]", "[add]", "[signature]", "[env]", "[precedence]", "[kb lpo|mpo|kbo]", "[sys]"]

repl :: IO ()
repl = do
    liftIO $ putStr "Commands: " 
    liftIO $ mapM_ (\s -> putStr (" " ++ s ++ " ")) commands >> putStrLn ""
    _ <- runRWST runRepl "term-rewriter" initReplEnv
    return ()

initReplEnv :: ReplEnv
initReplEnv = REnv {signature=[], curRewriteSystem = Rules [], currRules = [], curEquations = []}

runRepl :: ReplM ()
runRepl = do
    cEnv <- get
    c <- liftIO $ prompt (defaultPrompt cEnv)
    let (com:args) = words c
    liftIO $ putStrLn $ "Command: " ++ com ++ " Args: " ++ show args
    case com of --Very basic for now. 
        "exit" -> liftIO $ void $ putStrLn "Shutting down!"
        _ -> processCommand com args >> runRepl

-- I don't really like this. 
prompt :: String -> IO String
prompt p = do 
    putStr p 
    hFlush stdout 
    getLine

processCommand :: String -> [String] -> ReplM ()
processCommand command args = do 
    case command of 
        "add"        -> addEqn args
        "signature"  -> setSig args
        "env"        -> showEnv
        "precedence" -> setSig args
        "kb"         -> runKb args
        "sys"        -> gets curRewriteSystem >>= (liftIO . putStrLn . showTRS)
        _ -> liftIO $ putStrLn "Command not found."  

setSig :: [String] -> ReplM() 
setSig symbols = do 
    modify $ \env -> env {signature = symbols}

-- Currently very fragile. Terms cannot have spaces in them. 
addEqn :: [String] -> ReplM () 
addEqn e = do
    sig <- gets signature
    if null sig 
    then liftIO $ putStrLn "Error: Signature is empty."
    else do
        let (eqLHS, eqRHS) = (e !! 0, e !! 2)
        liftIO $ putStrLn $ "LHS: " ++ eqLHS ++ " RHS: " ++ eqRHS  
        let newEq = eqMap (getTerm (map head sig)) (eqLHS :~: eqRHS)
        modify $ \env -> env { curEquations = newEq:curEquations env }

showEnv :: ReplM () 
showEnv = do 
    env <- get 
    liftIO $ print env 

runKb :: [String] -> ReplM ()
runKb args = do
    eqsToComplete <- gets curEquations
    ord <- getOrdFromInput (head args) -- TODO: Head is an unsafe function, fix this.  
    let (result, complEnv, trace) = complete eqsToComplete ord
    liftIO $ mapM_ putStrLn trace
    case result of 
        Left _ -> return ()
        Right () -> modify $ \env -> env { curRewriteSystem = mkRewriteSystem $ map snd (markedRules complEnv) }

getOrdFromInput :: String -> ReplM TermOrder
getOrdFromInput arg = do 
    sig <- gets signature
    case arg of 
        "lpo" -> return $ lpo sig
        "mpo" -> return $ mpo sig
        "kbo" -> getKbOrder
        _ -> return $ lpo sig

getKbOrder :: ReplM TermOrder
getKbOrder = do 
    sig <- gets signature
    symWeights <- getSymWeights sig
    return $ kbo sig (weight (weightFromTuples symWeights))

getSymWeights :: OrderedSig -> ReplM [(FSym, Int)]
getSymWeights = mapM readSymWeight 

readSymWeight :: FSym -> ReplM (FSym, Int)
readSymWeight f = do 
    i <- liftIO $ prompt ("w(" ++ f ++ "): ")
    return (f, read i)

weightFromTuples :: [(FSym, Int)] -> FSym -> Int 
weightFromTuples symbolWeights f = case lookup f symbolWeights of
    Just i -> i 
    Nothing -> -1

-- This should be refactored utility for KB orders. 
weight :: (FSym -> Int) -> Term -> Int
weight _ (V _) = 1 
weight w (T f ts) = w f + sum (map (weight w) ts)

showTRS :: RewriteSystem -> String 
showTRS r = "\nRULES( \n" ++ concatMap (\s -> "\t" ++ show s ++ "\n") (rules r) ++ ")\n"  