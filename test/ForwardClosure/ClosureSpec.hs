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
      it "should compute the forward closure of the example term rewriting system" $ do
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