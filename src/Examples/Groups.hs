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
import Orders.KnuthBendixOrder    ( kbo, termWeight, weightFromTuples )
import Orders.PolyOrders          (lexOrd, multiOrder)
import Completion.CompletionUtils ( TermOrder )

groupSig :: OrderedSig
groupSig = ["1", "f", "i"]

symWeight :: FSym -> Int
symWeight = weightFromTuples [("1",1), ("i",0), ("f",0)]

groupTermParser :: String -> Term
groupTermParser = getTerm groupSig

getGroupAxioms :: [Equation Term Term]
getGroupAxioms = map (eqMap groupTermParser) [assoc, inv, id]
    where 
        assoc = "f(f(x,y),z)" :~: "f(x,f(y,z))"
        inv   = "f(i(x),x)" :~: "1"
        id    = "f(1,x)" :~: "x"

groupKBOrder :: TermOrder 
groupKBOrder = kbo groupSig (termWeight symWeight)

runGroupExample :: IO ()
runGroupExample = do 
    let (result, env, trace) = complete getGroupAxioms groupKBOrder
    mapM_ putStrLn trace
    mapM_ (print . snd) (markedRules env)