module Orders.PolyOrdersSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Orders.PolyOrders

instance Orderable Int where 
    order x y | x == y = E 
              | x < y  = NGE 
              | x > y  = GR

main :: IO ()
main = hspec $ do 
    describe "The PolyOrders Module" $ do 
        describe "The lexOrder implementation" $ do 
            it "should return GR on [1,1,2] >_lex [1,1,1]" $ do
                let x = ([1,1,2] :: [Int]) 
                let y = ([1,1,1] :: [Int])
                (lexOrd x y) `shouldBe` GR

