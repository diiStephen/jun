module Termination.TerminationChecker (
    TerminationTactic,
    RewriteRule (Rule),
    RewriteSystem,
    lpoTactic,
    mpoTactic,
    tactics,
    checkTermination
) where 

import Terms.Terms       ( Term(..), OrderedSig )
import Orders.TermOrders ( lpo, mpo )
import Orders.PolyOrders ( Order(..) )
import Data.List         ( all )

newtype TerminationTactic = Tactic { runTactic :: Term -> Term -> Bool }
data RewriteRule          = Rule { lhs :: Term, rhs :: Term }
newtype RewriteSystem     = Rules { rules :: [RewriteRule] }

lpoTactic :: OrderedSig -> RewriteRule -> Bool 
lpoTactic sig rule | lpo sig (lhs rule) (rhs rule) == GR = True
                   | otherwise = False

mpoTactic :: OrderedSig -> RewriteRule -> Bool 
mpoTactic sig rule | mpo sig (lhs rule) (rhs rule) == GR = True 
                   | otherwise = False

tactics :: OrderedSig -> RewriteRule -> Bool 
tactics sig rule = lpoTactic sig rule || mpoTactic sig rule 

checkTermination :: OrderedSig -> RewriteSystem -> Bool 
checkTermination sig rs = all (tactics sig) (rules rs)
