module Examples.CentralGroupoids (
    runCentralGroupoidExample
) where

import Terms.Terms                ( Term(..), OrderedSig )
import Equations.BasicEquation    ( Equation(..) )
import Terms.TermParser           ( getTerm )
import Orders.PolyOrders          ( Order(..) )
import Orders.RecursivePathOrders ( lpo )
import Completion.HuetCompletion  ( CompletionEnv(..), complete )

centralGroupoidSig :: OrderedSig 
centralGroupoidSig = ["f"]

parser :: String -> Term
parser = getTerm ['f']

getCGAxioms :: [Equation Term Term]
getCGAxioms = [cg]
    where
        cg = parser "f(f(x,y),f(y,z))" :~: parser "y"

getCGOrder :: Term -> Term -> Order
getCGOrder = lpo centralGroupoidSig

runCentralGroupoidExample :: IO ()
runCentralGroupoidExample = do 
    let (r, e, l) = complete getCGAxioms getCGOrder
    mapM_ putStrLn l
    print (markedRules e)