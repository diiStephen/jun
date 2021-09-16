module Orders.KnuthBendixOrderSpec (spec) where

import Test.Hspec                ( describe, it, shouldBe, Spec )
import Test.QuickCheck           ()
import Terms.TermParser          ( getTerm)
import Orders.KnuthBendixOrder   ( weightFromTuples, termWeight, kbo )
import Orders.PolyOrders         ( Order(..) )
import Terms.Terms               ( Term )

getGroupWeightFunction :: Term -> Int
getGroupWeightFunction = termWeight (weightFromTuples [("1", 1), ("f", 0), ("i", 1)])

spec :: Spec
spec = do
  describe "The KnuthBendixOrder module" $ do
    describe "the kbo function" $ do
      it "should return GR for the comparison f(i(x), x) > 1" $ do
        let sig = ["1", "f", "i"]
        let term = getTerm sig
        let lhs = term "f(i(x),x)"
        let rhs = term "1"
        let result = kbo sig getGroupWeightFunction lhs rhs
        result `shouldBe` GR

      it "should return GR for the comparison f(1,x) > x" $ do
        let sig = ["1", "f", "i"]
        let term = getTerm sig
        let lhs = term "f(1,x)"
        let rhs = term "x"
        let result = kbo sig getGroupWeightFunction lhs rhs
        result `shouldBe` GR

      it "should return GR for the comparison f(f(x,y),z) > f(x,f(y,z))" $ do
        let sig = ["1", "f", "i"]
        let term = getTerm sig
        let lhs = term "f(f(x,y),z)"
        let rhs = term "f(x,f(y,z))"
        let result = kbo sig getGroupWeightFunction lhs rhs
        result `shouldBe` GR


