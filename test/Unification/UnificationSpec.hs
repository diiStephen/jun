module Unification.UnificationSpec (spec) where 

import Test.Hspec              ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck         () 
import Control.Exception       (evaluate)
import Unification.Unification ( unify )
import Terms.TermParser        ( getTerm, parse, topLevel )
import Terms.Terms             ( Term(T) ) 

spec :: Spec 
spec = do 
    describe "The basic syntactic unification algorithm" $ do 
        it "should produce the mgu for the equations {x =^? f(a), g(x,x) =^? g(x,y)}" $ do 
            let sig = ["g", "f", "a"]
            let t1 = getTerm sig "g(x,x)"
            let t2 = getTerm sig "g(x,y)"
            let t3 = getTerm sig "x"
            let t4 = getTerm sig "f(a)"
            let unifProb = [(t3,t4), (t1,t2)]
            unify unifProb [] `shouldBe` Just [(('y',1),T "f" [T "a" []]),(('x',1),T "f" [T "a" []])]
        
        it "should produce Nothing for the equations {x =^? g(x)}" $ do 
            let t1 = getTerm ["g"] "g(x)"
            let t2 = getTerm [] "x"
            let unifProb = [(t2, t1)]
            unify unifProb [] `shouldBe` Nothing 

        it "should produce the mgu for the equations {f(x,y) =^? f(f(a,b),x)}" $ do 
            let sig = ["f", "a", "b"]
            let t1 = getTerm sig "f(x,y)"
            let t2 = getTerm sig "f(f(a,b),x)"
            let unifProb = [(t1,t2)] 
            unify unifProb [] `shouldBe` Just [(('y',1),T "f" [T "a" [],T "b" []]),(('x',1),T "f" [T "a" [],T "b" []])] 

        it "should produce Nothing for the equations {g(x) =^? f(y)}" $ do 
            let sig = ["f", "g"]
            let t1 = getTerm sig "g(x)"
            let t2 = getTerm sig "f(y)"
            let unifProb = [(t1,t2)]
            unify unifProb [] `shouldBe` Nothing