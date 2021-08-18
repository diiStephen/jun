module StringRewriting.StringRewritingSystemsSpec (spec) where

import Test.Hspec                             ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck                        () 
import Control.Exception                      ( evaluate )
import StringRewriting.StringRewritingSystems ( StringRewriteRule(..), normalize, stringToTerm )
import Terms.Terms                            ( Term(..) )

spec :: Spec 
spec = do 
    describe "The StringRewritingSystem module" $ do 
        describe "The normalize function" $ do 
            it "should normalize the string abab to cc modulo {ab -> c}" $ do 
                let srs = ["ab" :->: "c"]
                let expected = "cc"
                let result = normalize srs "abab"
                result `shouldBe` expected

            it "should normalize the string bbbbbac to abbac module {bbb -> a}" $ do 
                let srs = ["bbb" :->: "a"]
                let expected = "abbac"
                let result = normalize srs "bbbbbac"
                result `shouldBe` expected
    
        describe "The stringToTerm function" $ do 
            it "should translate the string abc to the term c(b(a(x)))" $ do 
                let string = "abc"
                let expectedTerm = T "c" [T "b" [T "a" [V ('x',1)]]]
                let result = stringToTerm string
                result `shouldBe` expectedTerm
            
            it "should translate the string a to the term a(x)" $ do 
                let string = "a"
                let expectedTerm = T "a" [V ('x',1)]
                let result = stringToTerm string
                result `shouldBe` expectedTerm

            it "should translate the empty string to the term x" $ do 
                let string = ""
                let expectedTerm = V ('x', 1)
                let result = stringToTerm string
                result `shouldBe` expectedTerm