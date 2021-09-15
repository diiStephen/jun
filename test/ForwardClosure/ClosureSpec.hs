module ForwardClosure.ClosureSpec (spec) where

import Test.Hspec                ( describe, it, shouldBe, Spec )
import Test.QuickCheck           ()
import TermRewriting.Rewrite     ( RewriteRule(..), RewriteSystem(..) )
import Terms.TermParser          ( getTerm )
import ForwardClosure.Closure    ( computeForwardClosure )
import qualified Data.Set as Set ( fromList )

spec :: Spec
spec = do
  describe "The Closure module" $ do
    describe "the computeFowardClosure function" $ do
      it "should compute the forward closure of example1 term rewriting system" $ do
        let sig = ["f", "s"]
        let rho1 = Rule (getTerm sig "f(s(x))") (getTerm sig "f(x)")
        let rho2 = Rule (getTerm sig "s(s(s(x)))") (getTerm sig "x")
        let ex = Rules [rho1, rho2]
        let rho1Expected = Rule (getTerm sig "f(s(x))") (getTerm sig "f(x)")
        let rho2Expected = Rule (getTerm sig "s(s(s(x)))") (getTerm sig "x")
        let rho3Expected = Rule (getTerm sig "f(s(s(x)))") (getTerm sig "f(x)")
        let expectedResult = Rules [rho1Expected, rho2Expected, rho3Expected]
        let result = computeForwardClosure 2 ex
        Set.fromList (rules result) `shouldBe` Set.fromList (rules expectedResult)

      it "should return the input system if the system is already forward closed" $ do
        let sig = ["f", "g", "b", "c", "i"]
        let rho1 = Rule (getTerm sig "f(x,i(x))") (getTerm sig "g(x)")
        let rho2 = Rule (getTerm sig "g(b)") (getTerm sig "c")
        let rho3 = Rule (getTerm sig "f(b,i(b))") (getTerm sig "c")
        let honoDefenseExample = Rules [rho1, rho2, rho3]
        let result = computeForwardClosure 2 honoDefenseExample
        result `shouldBe` honoDefenseExample