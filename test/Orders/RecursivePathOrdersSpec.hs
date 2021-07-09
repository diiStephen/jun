module Orders.RecursivePathOrdersSpec where 

import Test.Hspec                 ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck            () 
import Control.Exception          (evaluate)
import Orders.PolyOrders          ( Order(GR, NGE) )
import Orders.RecursivePathOrders ( lpo, mpo )
import Terms.TermParser           ( parse, topLevel )
import Terms.Terms                ( Term ) 

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

        describe "The lpo function on the ordered signature g < s < f" $ do 
            it "should return GR for f(s(x), s(y)) > f(g(g(x)), f(x, s(s(y))))" $ do 
                let sig = ['g', 's', 'f']
                let orderedSig = ["g", "s", "f"]
                let s = getTerm sig "f(s(x), s(y))"
                let t = getTerm sig "f(g(g(x)), f(x, s(s(y))))"
                lpo orderedSig s t `shouldBe` GR

            it "should return NGE for f(s(y), x) > s(f(x, s(s(y))))" $ do 
                let sig = ['g', 's', 'f']
                let orderedSig = ["g", "s", "f"]
                let s = getTerm sig "f(s(y), x)"
                let t = getTerm sig "s(f(x, s(s(y))))"
                lpo orderedSig s t `shouldBe` NGE

        describe "The mpo/lpo functions on the ordered signature f" $ do 
            it "mpo should return NGE for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let sig = ['f']
                let orderedSig = ["f"]
                let s = getTerm sig "f(f(x,y), z)"
                let t = getTerm sig "f(x, f(y,z))"
                mpo orderedSig s t `shouldBe` NGE 

            it "lpo should return GR for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let sig = ['f']
                let orderedSig = ["f"]
                let s = getTerm sig "f(f(x,y), z)"
                let t = getTerm sig "f(x, f(y,z))"
                lpo orderedSig s t `shouldBe` GR 