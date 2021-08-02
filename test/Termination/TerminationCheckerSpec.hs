module Termination.TerminationCheckerSpec (spec) where 

import Test.Hspec                     ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck                () 
import Control.Exception              ( evaluate )
import Termination.TerminationChecker ( evalTermination, checkLpoTermination, checkMpoTermination, TerminationError(..) ) 
import Terms.TermParser               ( getTerm )
import TermRewriting.Rewrite          ( RewriteRule(..), RewriteSystem(..) )

spec :: Spec
spec = do 
    describe "The TerminationChecker" $ do 
        describe "when given the R_ack TRS " $ do 
            it "should determine it is terminating with an lpo" $ do 
                let sig = ["0","s","a"] 
                
                -- a(0, y) --> s(y)
                let l1    = getTerm sig "a(0,y)"
                let r1    = getTerm sig "s(y)"
                let rule1 = Rule l1 r1 

                -- a(s(x), 0) --> a(x, s(0))
                let l2    = getTerm sig "a(s(x), 0)"
                let r2    = getTerm sig "a(x, s(0))"
                let rule2 = Rule l2 r2 

                -- a(s(x), s(y)) --> a(x, a(s(x), y))
                let l3    = getTerm sig "a(s(x), s(y))"
                let r3    = getTerm sig "a(x, a(s(x), y))"
                let rule3 = Rule l3 r3

                let trs = Rules [rule1, rule2, rule3]
                let orderedSig = ["s", "0", "a"]

                let result = evalTermination checkLpoTermination orderedSig trs
                fst result `shouldBe` Right True
        
        describe "when given a TRS with a commutativity axiom " $ do 
            it "should fail to determine it is terminating with mpo and lpo" $ do 

                -- f(x,y) --> f(y,x)
                let sig = ["f"]
                let l = getTerm sig "f(x,y)"
                let r = getTerm sig "f(y,x)"
                let r1 = Rule l r 
                let trs = Rules [r1] 

                let result1 = fst $ evalTermination checkLpoTermination ["f"] trs 
                let result2 = fst $ evalTermination checkMpoTermination ["f"] trs 

                result1 `shouldBe` Left (TFail "Rule f(x1,y1) --> f(y1,x1) cannot be proved terminating with lpo")
                result2 `shouldBe` Left (TFail "Rule f(x1,y1) --> f(y1,x1) cannot be proved terminating with mpo") 