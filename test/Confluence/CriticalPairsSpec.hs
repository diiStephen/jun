 module Confluence.CriticalPairsSpec where 

import Test.Hspec                     ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck                () 
import Control.Exception              ( evaluate )
import Terms.TermParser               ( getTerm )
import Terms.Terms                    ( Term(..) )
import Confluence.CriticalPairs       ( CriticalPair(..), criticalPair )
import TermRewriting.Rewrite          ( RewriteRule(..), mkDisjointVars )

main :: IO ()
main = hspec $ do 
    describe "The criticalPair function" $ do 
        describe "when given the rule f(f(x)) --> g(x), a renamed variant, and the position 1" $ do 
            it "should produce the critical pair <g(f(x)), f(g(x))>" $ do
                 let sig = ['f', 'g']
                 let s = getTerm sig "f(f(x))"
                 let t = getTerm sig "g(x)"
                 let rho = Rule s t
                 let p = "1"
                 let cp = criticalPair (mkDisjointVars rho rho) rho p 
                 let expectedCriticalPair = Just CP{left=getTerm sig "g(f(x))", right=getTerm sig "f(g(x))"}
                 cp `shouldBe` expectedCriticalPair

        describe "when given the rules f(x) --> g(x), h(x) --> g(x), and the position \"\"" $ do 
            it "should produce Nothing" $ do 
                let sig = ['f', 'g', 'h'] 

                let rho1 = Rule (getTerm sig "f(x)") (getTerm sig "g(x)")
                let rho2 = Rule (getTerm sig "h(x)") (getTerm sig "g(x)")

                let expectedCriticalPair = Nothing 
                let resultingCriticalPair = criticalPair (mkDisjointVars rho1 rho2) rho2 ""
                resultingCriticalPair `shouldBe` expectedCriticalPair