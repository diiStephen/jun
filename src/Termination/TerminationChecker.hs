module Termination.TerminationChecker (
    TerminationTactic,
    RewriteRule (Rule),
    RewriteSystem,
    lpoTactic,
    mpoTactic,
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

-- R is terminating iff \exists > , a reduction order on T(\Sigma, V) such that \forall l -> r \in R . l > r. 
-- For any (partial) order on \Sigma the induced lpo/mpo a simplification order, and therefore a reduction order. 
-- Note: The SAME reduction order must work for all rules in R. You cannot use differnt orders for each rule. 
-- This is due to the fact that termination is NOT a modular property of TRS's.  
checkTermination :: OrderedSig -> RewriteSystem -> Bool 
checkTermination sig rs = all (lpoTactic sig) (rules rs) || all (mpoTactic sig) (rules rs)
