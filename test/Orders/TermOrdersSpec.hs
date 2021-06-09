module Orders.TermOrdersSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Orders.TermOrders
import Terms.TermParser

getTerm :: [Char] -> String -> Term 
getTerm sig s = head $ fst $ parse (topLevel sig) s