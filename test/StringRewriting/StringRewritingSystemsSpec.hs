module StringRewriting.StringRewritingSystemsSpec (spec) where

import Test.Hspec                             ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck                        () 
import Control.Exception                      ( evaluate )
import StringRewriting.StringRewritingSystems ( StringRewriteRule(..), normalize, stringToTerm, stringToTerm2, reduce, rewriteAt )
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
        
        describe "The stringToTerm2 function" $ do
            it "should translate the string abc to the term c(b(a(x)))" $ do 
                let string = "abc"
                let expectedTerm = T "c" [T "b" [T "a" [V ('x',1)]]]
                let result = stringToTerm2 string
                result `shouldBe` expectedTerm
            
            it "should translate the string a to the term a(x)" $ do 
                let string = "a"
                let expectedTerm = T "a" [V ('x',1)]
                let result = stringToTerm2 string
                result `shouldBe` expectedTerm

            it "should translate the empty string to the term x" $ do 
                let string = ""
                let expectedTerm = V ('x', 1)
                let result = stringToTerm2 string
                result `shouldBe` expectedTerm

        describe "The reduce function" $ do 
            it "should contract the redex ccaab to ccc" $ do 
                let rule = "aab" :->: "c"
                let redex = "ccaab" 
                let expectedReduct = Just "ccc"
                let result = reduce rule redex 
                result `shouldBe` expectedReduct
            
            it "should return Nothing for the irreducible string bb" $ do 
                let rule = "aa" :->: "a"
                let irreducible = "bb"
                let expectedResult = Nothing 
                let result = reduce rule irreducible
                result `shouldBe` expectedResult
        
        describe "The rewrite at function" $ do 
            it "should reduce the string abba to aaa by rewriting at position 2" $ do 
                let rule = "bb" :->: "a"
                let string = "abba"
                let position = 2
                let expectedResult = "aaa"
                let result = rewriteAt rule string position
                result `shouldBe` expectedResult