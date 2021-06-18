module Confluence.CriticalPairs (
    CriticalPair (..),
    fpos
) where

import Terms.Terms            ( Term (..) )
import TermRewriting.Rewrite  ( RewriteSystem (..), RewriteRule (..) )
import Control.Monad.Identity ( Identity )
import Control.Monad.State    ( StateT )


data CriticalPair = CP { left :: Term, right :: Term }

instance Show CriticalPair where 
    show (CP l r) = "〈 " ++ show l ++ "," ++ show r ++ " 〉"

type CriticalPairs = [CriticalPair]

type Eval a = StateT CriticalPairs Identity a

overlap :: RewriteRule -> RewriteRule -> Eval a
overlap s t = undefined

fpos :: Term -> [Term]
fpos (V _)    = []
fpos (T f ts) = (:) (T f ts) (ts >>= fpos) 