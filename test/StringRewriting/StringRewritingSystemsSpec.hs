module StringRewriting.StringRewritingSystemsSpec (spec) where

import Test.Hspec                             ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck                        () 
import Control.Exception                      ( evaluate )
import StringRewriting.StringRewritingSystems ( StringRewriteRule(..), normalize )

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