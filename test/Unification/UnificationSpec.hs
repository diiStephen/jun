module Unification.UnificationSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Unification.Unification
import Terms.TermParser
import Terms.Terms 

main :: IO () 
main = hspec $ do 
    describe "The basic syntactic unification algorithm" $ do 
        it "should produce a correct mgu given the equations" $ do 
            let t1 = fst $ head $ parse (topLevel ['g', 'a']) "g(x,x)"
            let t2 = fst $ head $ parse (topLevel ['g', 'a']) "g(x,y)"
            let t3 = fst $ head $ parse (topLevel ['g', 'a']) "x"
            let t4 = fst $ head $ parse (topLevel ['f', 'a']) "f(a)"
            let unifProb = [(t3,t4), (t1,t2)]
            (unify unifProb []) `shouldBe` Just [(('y',1),T "f" [T "a" []]),(('x',1),T "f" [T "a" []])]
            