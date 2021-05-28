module Orders.PolyOrdersSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Orders.PolyOrders

instance Orderable Int where 
    order x y | x == y = E 
              | x < y  = NGE 
              | x > y  = GR

