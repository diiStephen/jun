module Orders.PolyOrdersSpec where 

import Test.Hspec        ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck   () 
import Control.Exception (evaluate)
import Orders.PolyOrders ( lexOrd, multiOrder, Order(NGE, GR, E) )

order :: Ord a => a -> a -> Order
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
                lexOrd order x y `shouldBe` GR

            it "should return NGE on [1,1] >_lex [2,2]" $ do 
                let x = ([1,1] :: [Int])
                let y = ([2,2] :: [Int])
                lexOrd order x y `shouldBe` NGE 
            
            it "should return E on [3,3,3] >_lex [3,3,3]" $ do 
                let x = ([3,3,3] :: [Int])
                let y = ([3,3,3] :: [Int])
                lexOrd order x y `shouldBe` E 
        
        describe "The multiOrder implementation" $ do 
            it "should return GR on [5,3,1,1] >_mul [4,3,3,1]" $ do 
                let x = ([5,3,1,1] :: [Int])
                let y = ([4,3,3,1] :: [Int])
                multiOrder order x y `shouldBe` GR 

            it "should return E on [1] >_mul [1]" $ do 
                let x = ([1] :: [Int])
                let y = ([1] :: [Int])
                multiOrder order x y `shouldBe` E
            
            it "should return NGE on [6,4,4,1] >_mul [7,4,4,1]" $ do 
                let x = ([6,4,4,1] :: [Int])
                let y = ([7,4,4,1] :: [Int])
                multiOrder order x y `shouldBe` NGE