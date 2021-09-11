module ForwardClosure.ClosureSpec (spec) where

import Test.Hspec             ( describe, it, shouldBe, Spec )
import Test.QuickCheck        ()
import TermRewriting.Rewrite  ( RewriteRule(..), RewriteSystem(..) )
import Terms.TermParser       ( getTerm )
import ForwardClosure.Closure ( computeForwardClosure )

spec :: Spec
spec = do
  describe "The Closure module" $ do
    describe "the computeFowardClosure function" $ do
      it "should compute the forward closure of the example term rewriting system" $ do
        let sig = ["f", "s"]
        let rho1 = Rule (getTerm sig "f(s(x))") (getTerm sig "f(x)")
        let rho2 = Rule (getTerm sig "s(s(s(x)))") (getTerm sig "x")
        let ex = Rules [rho1, rho2]
        let result = computeForwardClosure 2 ex
        let resultSize = length (rules result)
        resultSize `shouldBe` 3
        print result