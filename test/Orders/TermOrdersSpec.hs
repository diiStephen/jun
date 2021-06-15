module Orders.TermOrdersSpec where 

import Test.Hspec        ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck   () 
import Control.Exception (evaluate)
import Orders.PolyOrders ( Order(GR) )
import Orders.TermOrders ( lpo )
import Terms.TermParser  ( parse, topLevel )
import Terms.Terms       ( Term ) 

getTerm :: [Char] -> String -> Term 
getTerm sig s = fst $ head $ parse (topLevel sig) s

main :: IO () 
main = hspec $ do 
    describe "The TermOrders module " $ do 
        describe "The lpo function on the orderd signature e < f < i" $ do 
            it "should return GR for f(x,e) > x" $ do 
                let sig = ['e', 'f', 'i']
                let orderedSig = ["e", "f", "i"]
                let s = getTerm sig "f(x,e)"
                let t = getTerm sig "x"
                lpo orderedSig s t `shouldBe` GR
            
            it "should return GR for i(e) > e" $ do
                let sig = ['e', 'f', 'i']
                let orderedSig = ["e", "f", "i"] 
                let s = getTerm sig "i(e)"
                let t = getTerm sig "e"
                lpo orderedSig s t `shouldBe` GR 

            it "should return GR for i(f(x,y)) > f(i(x), i(y))" $ do
                let sig = ['e', 'f', 'i']
                let orderedSig = ["e", "f", "i"]
                let s = getTerm sig "i(f(x,y))"
                let t = getTerm sig "f(i(x), i(y))"
                lpo orderedSig s t `shouldBe` GR 

            it "should return GR for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let sig = ['e', 'f', 'i']
                let orderedSig = ["e", "f", "i"]
                let s = getTerm sig "f(f(x,y), z)"
                let t = getTerm sig "f(x,f(y,z))"
                lpo orderedSig s t `shouldBe` GR  



