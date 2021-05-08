module Terms.TermParserSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Terms.TermParser 
import Terms.Terms 

main :: IO ()
main = hspec $ do 
    describe "The term parser topLevel parser" $ do 
        it "should parse a string representing the term f(x,y)" $ do 
            parse (topLevel ['f']) "f(x,y)" `shouldBe` [(T "f" [V ('x',1),V ('y',1)], "")] 
        
        it "should parse a string representing the term f(f(a,b),x)" $ do 
            parse (topLevel ['f', 'a', 'b']) "f(f(a,b),x)" `shouldBe` [(T "f" [T "f" [T "a" [], T "b" []], V ('x',1)], "")]

        it "should parse a string representing the term x" $ do 
            parse (topLevel []) "x" `shouldBe` [(V ('x',1), "")]

