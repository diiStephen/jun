module Examples.Groups (
    runGroupExample
) where 

import Terms.Terms                ( Term(..), OrderedSig )
import Terms.TermParser           ( getTerm )
import Equations.BasicEquation    ( Equation(..) )
import Completion.HuetCompletion  ( complete )
import Orders.RecursivePathOrders ( rpo, lpo, mpo )
import Orders.PolyOrders          (lexOrd, multiOrder)
import Completion.CompletionUtils ( TermOrder )

groupSig :: OrderedSig
groupSig = ["1", "i", "f"]

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

runGroupExample :: IO ()
runGroupExample = do 
    let (result, env, trace) = complete getGroupAxioms getGroupOrder
    mapM_ putStrLn trace