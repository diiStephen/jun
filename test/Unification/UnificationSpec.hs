module Unification.UnificationSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Unification.Unification
import Terms.TermParser
import Terms.Terms 

termGenerator :: [Char] -> String -> Term 
termGenerator sig s = fst $ head $ parse (topLevel sig) s

main :: IO () 
main = hspec $ do 
    describe "The basic syntactic unification algorithm" $ do 
        it "should produce the mgu for the equations {x =^? f(a), g(x,x) =^? g(x,y)}" $ do 
            let sig = ['g', 'f', 'a']
            let t1 = termGenerator sig "g(x,x)"
            let t2 = termGenerator sig "g(x,y)"
            let t3 = termGenerator sig "x"
            let t4 = termGenerator sig "f(a)"
            let unifProb = [(t3,t4), (t1,t2)]
            (unify unifProb []) `shouldBe` Just [(('y',1),T "f" [T "a" []]),(('x',1),T "f" [T "a" []])]
        
        it "should produce Nothing for the equations {x =^ g(x)}" $ do 
            let t1 = termGenerator ['g'] "g(x)"
            let t2 = termGenerator [] "x"
            let unifProb = [(t2, t1)]
            (unify unifProb []) `shouldBe` Nothing 

        it "should produce the mgu for the equations {f(x,y) = f(f(a,b),x)}" $ do 
            let sig = ['f', 'a', 'b']
            let t1 = termGenerator sig "f(x,y)"
            let t2 = termGenerator sig "f(f(a,b),x)"
            let unifProb = [(t1,t2)] 
            (unify unifProb []) `shouldBe` Just [(('y',1),T "f" [T "a" [],T "b" []]),(('x',1),T "f" [T "a" [],T "b" []])] 

        it "should produce Nothing for the equations {g(x) = f(y)}" $ do 
            let sig = ['f', 'g']
            let t1 = termGenerator sig "g(x)"
            let t2 = termGenerator sig "f(y)"
            let unifProb = [(t1,t2)]
            (unify unifProb []) `shouldBe` Nothing