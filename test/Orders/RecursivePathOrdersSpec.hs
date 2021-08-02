module Orders.RecursivePathOrdersSpec (spec) where 

import Test.Hspec                 ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck            () 
import Control.Exception          (evaluate)
import Orders.PolyOrders          ( Order(GR, NGE) )
import Orders.RecursivePathOrders ( lpo, mpo )
import Terms.TermParser           ( parse, topLevel, getTerm )
import Terms.Terms                ( Term ) 

spec :: Spec 
spec = do 
    describe "The TermOrders module " $ do 
        describe "The lpo function on the orderd signature e < f < i" $ do 
            it "should return GR for f(x,e) > x" $ do 
                let orderedSig = ["e", "f", "i"]
                let s = getTerm orderedSig "f(x,e)"
                let t = getTerm orderedSig "x"
                lpo orderedSig s t `shouldBe` GR
            
            it "should return GR for i(e) > e" $ do
                let orderedSig = ["e", "f", "i"] 
                let s = getTerm orderedSig "i(e)"
                let t = getTerm orderedSig "e"
                lpo orderedSig s t `shouldBe` GR 

            it "should return GR for i(f(x,y)) > f(i(x), i(y))" $ do
                let orderedSig = ["e", "f", "i"]
                let s = getTerm orderedSig "i(f(x,y))"
                let t = getTerm orderedSig "f(i(x), i(y))"
                lpo orderedSig s t `shouldBe` GR 

            it "should return GR for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let orderedSig = ["e", "f", "i"]
                let s = getTerm orderedSig "f(f(x,y), z)"
                let t = getTerm orderedSig "f(x,f(y,z))"
                lpo orderedSig s t `shouldBe` GR  

        describe "The lpo function on the ordered signature g < s < f" $ do 
            it "should return GR for f(s(x), s(y)) > f(g(g(x)), f(x, s(s(y))))" $ do 
                let orderedSig = ["g", "s", "f"]
                let s = getTerm orderedSig "f(s(x), s(y))"
                let t = getTerm orderedSig "f(g(g(x)), f(x, s(s(y))))"
                lpo orderedSig s t `shouldBe` GR

            it "should return NGE for f(s(y), x) > s(f(x, s(s(y))))" $ do 
                let orderedSig = ["g", "s", "f"]
                let s = getTerm orderedSig "f(s(y), x)"
                let t = getTerm orderedSig "s(f(x, s(s(y))))"
                lpo orderedSig s t `shouldBe` NGE

        describe "The mpo/lpo functions on the ordered signature f" $ do 
            it "mpo should return NGE for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let orderedSig = ["f"]
                let s = getTerm orderedSig "f(f(x,y), z)"
                let t = getTerm orderedSig "f(x, f(y,z))"
                mpo orderedSig s t `shouldBe` NGE 

            it "lpo should return GR for f(f(x,y), z) > f(x, f(y,z))" $ do 
                let orderedSig = ["f"]
                let s = getTerm orderedSig "f(f(x,y), z)"
                let t = getTerm orderedSig "f(x, f(y,z))"
                lpo orderedSig s t `shouldBe` GR 