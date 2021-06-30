 module Confluence.CriticalPairsSpec where 

import Test.Hspec                     ( hspec, describe, it, shouldBe ) 
import Test.QuickCheck                () 
import Control.Exception              ( evaluate )
import Terms.TermParser               ( getTerm )
import Terms.Terms                    ( Term(..), alphaConvert, set )
import Confluence.CriticalPairs       ( CriticalPair(..), criticalPair, criticalPairs )
import TermRewriting.Rewrite          ( RewriteRule(..), mkDisjointVars )

main :: IO ()
main = hspec $ do 
    describe "The criticalPair function" $ do 
        describe "when given the rule f(f(x)) --> g(x), a renamed variant, and the position 1" $ do 
            it "should produce the critical pair <g(f(x)), f(g(x))>" $ do
                 let term = getTerm ['f', 'g']

                 let rho = Rule{ lhs=term "f(f(x))", rhs=term "g(x)" }

                 let expectedCriticalPair = Just CP{ left=term "g(f(x))", right=term "f(g(x))" }
                 let resultingCriticalPair = criticalPair (mkDisjointVars rho rho) rho "1" 
                 resultingCriticalPair `shouldBe` expectedCriticalPair

        describe "when given the rules f(x) --> g(x), h(x) --> g(x), and the position \"\"" $ do 
            it "should produce Nothing" $ do 
                let term = getTerm ['f', 'g', 'h']

                let rho1 = Rule{ lhs=term "f(x)", rhs=term "g(x)" }
                let rho2 = Rule{ lhs=term "h(x)", rhs=term "g(x)" }

                let expectedCriticalPair = Nothing 
                let resultingCriticalPair = criticalPair (mkDisjointVars rho1 rho2) rho2 ""
                resultingCriticalPair `shouldBe` expectedCriticalPair
    
    describe "The criticalPairs function" $ do 
        describe "when given the rules f(f(x,y),z) --> f(x,f(y,z)), f(x,1) --> x" $ do 
            it "should produce a list of all critical pairs found by overlapping the first rule with the second" $ do 
                let term = getTerm ['f', '1']

                let rho1 = Rule{ lhs=term "f(f(x,y),z)", rhs=term "f(x,f(y,z))" }
                let rho2 = Rule{ lhs=term"f(x,1)", rhs=term "x" }

                let expectedCriticalPairs = 
                        [ Just CP{ left=alphaConvert 2 (term "f(x,f(y,1))"), right=alphaConvert 2 (term "f(x,y)") }
                        , Just CP{ left=set (alphaConvert 2 (term "f(x,f(1,z))")) (V ('x',1)) "1", right=set (alphaConvert 2 (term "f(x,z)")) (V ('x',1)) "1" } 
                        ]
                        
                let resultingCriticalPairs = criticalPairs rho1 rho2 
                resultingCriticalPairs `shouldBe` expectedCriticalPairs