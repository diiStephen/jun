module Examples.Groups (
      runGroupExample
    , getGroupAxioms
    , groupKBOrder
) where 

import Terms.Terms                ( Term(..), OrderedSig, FSym )
import Terms.TermParser           ( getTerm )
import Equations.BasicEquation    ( Equation(..) )
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
groupTermParser = getTerm ['1', 'i', 'f']

getGroupAxioms :: [Equation Term Term]
getGroupAxioms = [assoc, inv, id]
    where 
        assoc = groupTermParser "f(f(x,y),z)" :~: groupTermParser "f(x,f(y,z))"
        inv   = groupTermParser "f(i(x),x)" :~: groupTermParser "1"
        id    = groupTermParser "f(1,x)" :~: groupTermParser "x"

getGroupOrder :: TermOrder
getGroupOrder = rpo groupSig groupStat 
    where 
        groupStat sym | sym == "f" = lexOrd
                      | otherwise = multiOrder

groupKBOrder :: TermOrder 
groupKBOrder = kbo groupSig weight

runGroupExample :: IO ()
runGroupExample = do 
    let (result, env, trace) = complete getGroupAxioms groupKBOrder
    mapM_ putStrLn trace
    mapM_ (print . snd) (markedRules env)