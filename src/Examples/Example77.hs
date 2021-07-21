module Examples.Example77 (
    runExample77
) where

import Terms.Terms                ( Term(..), OrderedSig )
import Terms.TermParser           ( getTerm )
import Equations.BasicEquation    ( Equation(..) )
import Completion.CompletionUtils ( TermOrder )
import Orders.RecursivePathOrders ( lpo )
import Completion.HuetCompletion  ( complete, CompletionEnv (markedRules) )

example77Sig :: OrderedSig 
example77Sig = ["f"]

p :: String -> Term
p = getTerm ['f']

example77Axioms :: [Equation Term Term]
example77Axioms = [invol, ax3, assoc]
    where 
        assoc = p "f(f(x,y),z)" :~: p "f(x,f(y,z))"
        invol = p "f(x,x)" :~: p "x"
        ax3   = p "f(f(x,y),x)" :~: p "x"

example77Order :: TermOrder 
example77Order = lpo example77Sig

runExample77 :: IO ()
runExample77 = do 
    let (result, finalEnv, trace) = complete example77Axioms example77Order
    mapM_ putStrLn trace
    mapM_ print (markedRules finalEnv)