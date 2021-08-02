module Examples.Groups (
      runGroupExample
    , getGroupAxioms
    , groupKBOrder
) where 

import Terms.Terms                ( Term(..), OrderedSig, FSym )
import Terms.TermParser           ( getTerm )
import Equations.BasicEquation    ( Equation(..), eqMap )
import Completion.HuetCompletion  ( CompletionEnv(..), complete )
import Orders.RecursivePathOrders ( rpo, lpo, mpo )
import Orders.KnuthBendixOrder    ( kbo )
import Orders.PolyOrders          (lexOrd, multiOrder)
import Completion.CompletionUtils ( TermOrder )

groupSig :: OrderedSig
groupSig = ["1", "f", "i"]

weight :: Term -> Int
weight (V x) = 1 
weight (T f ts) = symWeight f + sum (map weight ts)

symWeight :: FSym -> Int
symWeight f | f == "1" = 1
            | f == "i" = 0 
            | f == "f" = 0
            | otherwise = 0

groupTermParser :: String -> Term
groupTermParser = getTerm groupSig

getGroupAxioms :: [Equation Term Term]
getGroupAxioms = map (eqMap groupTermParser) [assoc, inv, id]
    where 
        assoc = "f(f(x,y),z)" :~: "f(x,f(y,z))"
        inv   = "f(i(x),x)" :~: "1"
        id    = "f(1,x)" :~: "x"

groupKBOrder :: TermOrder 
groupKBOrder = kbo groupSig weight

runGroupExample :: IO ()
runGroupExample = do 
    let (result, env, trace) = complete getGroupAxioms groupKBOrder
    mapM_ putStrLn trace
    mapM_ (print . snd) (markedRules env)