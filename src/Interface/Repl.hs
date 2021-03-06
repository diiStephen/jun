{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}

module Interface.Repl (
    repl
) where

import Terms.Terms                ( Term(..), OrderedSig, FSym )
import Terms.TermParser           ( getTerm )
import TermRewriting.Rewrite      ( RewriteSystem(..), RewriteRule(..), mkRewriteSystem, normalize )
import Equations.BasicEquation    ( Equation(..), eqMap )
import Completion.HuetCompletion  ( CompletionEnv(..), complete)
import Completion.CompletionUtils ( TermOrder )
import Orders.RecursivePathOrders ( lpo, mpo )
import Orders.KnuthBendixOrder    ( kbo, termWeight, weightFromTuples )
import ForwardClosure.Closure     ( computeForwardClosure )
import Control.Monad              ( void )
import Control.Monad.RWS          ( RWST, liftIO, gets, runRWST, modify, get )
import System.IO                  ( hFlush, stdout ) 
import Data.List                  ( intercalate )
import Data.Char                  ( isSpace )
import Data.Bifunctor             ( second )

type Log = [String]

type ReplM a = (RWST String Log ReplEnv) IO a

data ReplEnv = REnv {
      signature :: OrderedSig
    , curRewriteSystem :: RewriteSystem
    , curRules :: [RewriteRule] 
    , curEquations :: [Equation Term Term] 
} deriving (Show)

defaultPrompt :: ReplEnv -> String
defaultPrompt env = "{" ++ intercalate "," (signature env) ++ "}"
    ++ "[E " ++ show (length $ curEquations env) ++ "]" 
    ++ "[R " ++ show (length $ rules $ curRewriteSystem env) ++ "]" 
    ++ ":> "

commands :: [String]
commands = ["[exit]"
    , "[add]"
    , "[signature]"
    , "[env]"
    , "[precedence]"
    , "[complete lpo|mpo|kbo]"
    , "[sys]"
    , "[eqs]"
    , "[clear-eqs]"
    , "[norm]"
    , "[fc Int]"
    , "[help]"]

repl :: IO ()
repl = do
    liftIO $ putStr "Commands: " 
    liftIO $ mapM_ (\s -> putStr (" " ++ s ++ " ")) commands >> putStrLn ""
    _ <- runRWST runRepl "term-rewriter" initReplEnv
    return ()

initReplEnv :: ReplEnv
initReplEnv = REnv {signature=[], curRewriteSystem = Rules [], curRules = [], curEquations = []}

runRepl :: ReplM ()
runRepl = do
    cEnv <- get
    c <- liftIO $ prompt (defaultPrompt cEnv)
    let (com,args) = break isSpace c
    case com of
        "exit" -> liftIO $ void $ putStrLn "Shutting down!"
        _ -> processCommand com args >> runRepl
 
prompt :: String -> IO String
prompt p = do 
    putStr p 
    hFlush stdout 
    getLine

processCommand :: String -> String -> ReplM ()
processCommand command args = do 
    case command of 
        "add"        -> addEqn args
        "signature"  -> setSig args
        "env"        -> showEnv
        "precedence" -> setSig args
        "complete"   -> runKb args
        "sys"        -> gets curRewriteSystem >>= (liftIO . putStrLn . showSet "RULES" . rules)
        "eqs"        -> gets curEquations >>= (liftIO. putStrLn . showSet "EQUATIONS")
        "norm"       -> replNormalize args >>= (liftIO . putStrLn . showSet "NORMALIZED" . (:[]))
        "clear-eqs"  -> modify $ \env -> env { curEquations = [] }
        "fc"         -> runFC (read args)
        "help"       -> liftIO $ mapM_ (\s -> putStr (" " ++ s ++ " ")) commands >> putStrLn ""
        _ -> liftIO $ putStrLn "Command not found."  

setSig :: String -> ReplM () 
setSig symbols = do 
    modify $ \env -> env {signature = words symbols}

addEqn :: String -> ReplM () 
addEqn e = do
    sig <- gets signature
    if null sig 
    then liftIO $ putStrLn "Error: Signature is empty."
    else do
        let (eqLHS, eqRHS) = second (drop 1) . break (=='=') $ e  
        let newEq = eqMap (getTerm sig) (eqLHS :~: eqRHS)
        modify $ \env -> env { curEquations = newEq:curEquations env }

showEnv :: ReplM () 
showEnv = do 
    env <- get 
    liftIO $ print env 

runKb :: String -> ReplM ()
runKb args = do
    eqsToComplete <- gets curEquations
    ord <- getOrdFromInput $ drop 1 args
    let (result, complEnv, trace) = complete eqsToComplete ord
    liftIO $ mapM_ putStrLn trace
    case result of 
        Left _ -> return ()
        Right () -> modify $ \env -> env { curRewriteSystem = mkRewriteSystem $ map snd (markedRules complEnv), curEquations = [] }

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
    return $ kbo sig (termWeight (weightFromTuples symWeights))

getSymWeights :: OrderedSig -> ReplM [(FSym, Int)]
getSymWeights = mapM readSymWeight 

readSymWeight :: FSym -> ReplM (FSym, Int)
readSymWeight f = do 
    i <- liftIO $ prompt ("w(" ++ f ++ "): ")
    return (f, read i)

showSet :: (Foldable t, Show a) => String -> t a -> [Char]
showSet name es = "\n" ++ name ++ "(\n" ++ concatMap (\s -> "\t" ++ show s ++ "\n") es ++ ")\n" 

replNormalize :: String -> ReplM Term
replNormalize st = do 
    rewriteSystem <- gets curRewriteSystem
    sig <- gets signature
    let terms = getTerm sig st
    return $ normalize rewriteSystem terms

runFC :: Int -> ReplM ()
runFC steps = do
  rewriteSystem <- gets curRewriteSystem
  let fcResult = computeForwardClosure steps rewriteSystem
  modify $ \env -> env { curRewriteSystem = fcResult }
