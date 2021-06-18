module Confluence.CriticalPairs (
    CriticalPair (..),
) where

import Terms.Terms ( Term (..) )

data CriticalPair = CP { left :: Term, right :: Term }

instance Show CriticalPair where 
    show (CP l r) = "〈 " ++ show l ++ "," ++ show r ++ " 〉"
