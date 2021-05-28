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

            it "should return NGE on [1,1] >_lex [2,2]" $ do 
                let x = ([1,1] :: [Int])
                let y = ([2,2] :: [Int])
                (lexOrd x y) `shouldBe` NGE 
            
            it "should return E on [3,3,3] >_lex [3,3,3]" $ do 
                let x = ([3,3,3] :: [Int])
                let y = ([3,3,3] :: [Int])
                (lexOrd x y) `shouldBe` E 
        
        describe "The multiOrder implementation" $ do 
            it "should return GR on [5,3,1,1] >_mul [4,3,3,1]" $ do 
                let x = ([5,3,1,1] :: [Int])
                let y = ([4,3,3,1] :: [Int])
                (multiOrder x y) `shouldBe` GR 