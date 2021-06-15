module Orders.MonadicTermOrdersSpec where 

import Test.Hspec               ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck          () 
import Control.Exception        ( evaluate )
import Orders.PolyOrders        ( Order(GR, NGE) )
import Orders.MonadicTermOrders ( mLpo )
import Terms.TermParser         ( parse, topLevel )
import Terms.Terms              ( Term ) 
import Control.Monad.Reader     ( runReader )

getTerm :: [Char] -> String -> Term 
getTerm sig s = fst $ head $ parse (topLevel sig) s

main :: IO () 
main = hspec $ do 
    describe "The MonadicTermOrders module " $ do
        describe "The mLpo function with ordered signature 0 < s < f" $ do 
            it "should return GR for f(0,x) > s(x) " $ do 
                let sig = ['0', 's', 'f']
                let orderedSig = ["0", "s", "f"]
                let s = getTerm sig "f(0,x)"
                let t = getTerm sig "s(x)"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR

            it "should return GR for f(s(x), 0) > f(x, s(0)" $ do 
                let sig = ['0', 's', 'f']
                let orderedSig = ["0", "s", "f"]
                let s = getTerm sig "f(s(x),0)"
                let t = getTerm sig "f(x, s(0))"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR

            it "should return GR for f(s(x), s(y)) > f(x, f(s(x), y))" $ do 
                let sig = ['0', 's', 'f']
                let orderedSig = ["0", "s", "f"]
                let s = getTerm sig "f(s(x), s(y))"
                let t = getTerm sig "f(x, f(s(x), y))"
                let res = runReader (mLpo s t) orderedSig 
                res `shouldBe` GR