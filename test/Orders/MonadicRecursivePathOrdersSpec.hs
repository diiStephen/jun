module Orders.MonadicRecursivePathOrdersSpec (spec) where 

import Test.Hspec                        ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck                   () 
import Control.Exception                 ( evaluate )
import Orders.PolyOrders                 ( Order(GR, NGE) )
import Orders.MonadicRecursivePathOrders ( mLpo )
import Terms.TermParser                  ( parse, topLevel, getTerm )
import Terms.Terms                       ( Term ) 
import Control.Monad.Reader              ( runReader )

spec :: Spec
spec = do 
    describe "The MonadicTermOrders module " $ do
        describe "The mLpo function with ordered signature 0 < s < f" $ do 
            it "should return GR for f(0,x) > s(x) " $ do 
                let orderedSig = ["0", "s", "f"]
                let s = getTerm orderedSig "f(0,x)"
                let t = getTerm orderedSig "s(x)"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR

            it "should return GR for f(s(x), 0) > f(x, s(0)" $ do 
                let orderedSig = ["0", "s", "f"]
                let s = getTerm orderedSig "f(s(x),0)"
                let t = getTerm orderedSig "f(x, s(0))"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR

            it "should return GR for f(s(x), s(y)) > f(x, f(s(x), y))" $ do 
                let orderedSig = ["0", "s", "f"]
                let s = getTerm orderedSig "f(s(x), s(y))"
                let t = getTerm orderedSig "f(x, f(s(x), y))"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR