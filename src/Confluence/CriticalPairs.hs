module Confluence.CriticalPairs (
    CriticalPair (..),
    fpos
) where

import Terms.Terms                ( Term (..), alphaConvert, maxIndex, subterms )
import TermRewriting.Rewrite      ( RewriteSystem (..), RewriteRule (..) )
import Unification.Unification    ( unify' )
import Substitution.Substitutions (Subst, applyLifted)
import Control.Monad.Identity     ( Identity )
import Control.Monad.State        ( StateT )


data CriticalPair = CP { left :: Term, right :: Term }

instance Show CriticalPair where 
    show (CP l r) = "〈 " ++ show l ++ "," ++ show r ++ " 〉"

type CriticalPairs = [CriticalPair]

type Eval a = StateT CriticalPairs Identity a

-- s = l_1 --> r_1, t = l_2 --> r_2.
-- Try to unify (l_1)|p =^? l_2 , p \in FPos(l_1)
-- Critical Pair: (\theta(r_1), \theta(l_1)[\theta(r_2)]|p) 
--overlap :: Term - > Term -> Eval a
overlap :: Term -> Term -> [Maybe Subst]
overlap s t = map (unify' t) (fpos s) 

criticalPair :: RewriteRule  -> RewriteRule -> CriticalPairs
criticalPair rho tau = undefined 

fpos ::Term -> [Term]
fpos (V _)    = []
fpos (T f ts) = (:) (T f ts) (ts >>= fpos)