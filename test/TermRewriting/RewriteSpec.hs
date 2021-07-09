module TermRewriting.RewriteSpec where

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import TermRewriting.Rewrite
import Terms.Terms 
import Terms.TermParser

termGenerator :: [Char] -> String -> Term 
termGenerator sig s = fst $ head $ parse (topLevel sig) s

main :: IO ()
main = hspec $ do 
    describe "The TermRewriting module" $ do 
        describe "the normalize function given the TRS f(x,y) -> g(x,y)" $ do 
            it "should normalize the term f(f(a,b), f(a,b)) to g(g(a,b), g(a,b))" $ do
                let sig = ['f', 'g', 'a', 'b']
                let lhs = termGenerator sig "f(x,y)"
                let rhs = termGenerator sig "g(x,y)"
                let trs = Rules [Rule lhs rhs]
                let t   = termGenerator sig "f(f(a,b), f(a,b))"
                (normalize trs t) `shouldBe` T "g" [T "g" [T "a" [], T "b" []], T "g" [T "a" [], T "b" []]]