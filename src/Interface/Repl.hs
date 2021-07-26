{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}

module Interface.Repl (
    repl
) where

import Terms.Terms                ( Term(..), OrderedSig )
import Terms.TermParser           ( getTerm )
import TermRewriting.Rewrite      ( RewriteSystem(..), RewriteRule(..), mkRewriteSystem )
import Equations.BasicEquation    ( Equation(..), eqMap )
import Completion.HuetCompletion  (CompletionEnv(..), complete)
import Orders.RecursivePathOrders ( lpo )
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

defaultPrompt :: String
defaultPrompt = ":> "

commands :: [String]
commands = ["exit", "add", "signature", "env", "precedence", "kb"]

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
    c <- liftIO $ prompt defaultPrompt
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
    sig <- gets signature 
    let (result, complEnv, trace) = complete eqsToComplete (lpo sig)
    liftIO $ mapM_ putStrLn trace
    case result of 
        Left _ -> return ()
        Right () -> modify $ \env -> env { curRewriteSystem = mkRewriteSystem $ map snd (markedRules complEnv) }