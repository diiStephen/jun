module RewriteTester where 

import TermRewriting.Rewrite
import Terms.Terms

x = V ('x', 1)
y = V ('y', 1)
lhs = T "f" [x,y]
rhs = T "g" [x, y]

r1 = (lhs, rhs)

a = T "a" []
b = T "b" []
p = T "f" [a, b]

p2 = T "f" [p3, p3]
p3 = T "f" [a, b]

nf = normalize2 [r1] p2 