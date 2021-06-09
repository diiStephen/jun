module Orders.TermOrdersSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Orders.TermOrders
import Terms.TermParser
import Terms.Terms 

getTerm :: [Char] -> String -> Term 
getTerm sig s = fst $ head $ parse (topLevel sig) s