module UnifTest (
    doTest,
    unifProb,
    doTestMatch, 
    matchProb
) where 

import Terms.Terms
import Substitution.Substitutions
import Unification.Unification

x = V ('x', 1)
y = V ('y', 1)
a = T "a" []
t1 = T "f" [a]
t2 = T "g" [x,x]
t3 = T "g" [x,y]

unifProb = [(x,t1), (t2,t3)]

z = V ('z', 1)
t4 = T "f" [x, y]
t5 = T "g" [z]
t6 = T "f" [t5, x]

matchProb = [(t4, t6)]

doTest = (unify unifProb [])
doTestMatch = (match matchProb [])