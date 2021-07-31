module Terms.TermParserSpec (spec) where 

import Test.Hspec        ( hspec, describe, it, shouldBe, Spec ) 
import Test.QuickCheck   () 
import Control.Exception (evaluate)
import Terms.TermParser  ( parse, topLevel ) 
import Terms.Terms       ( Term(V, T) ) 

spec :: Spec
spec = do 
    describe "The term parser topLevel parser" $ do 
        it "should parse a string representing the term f(x,y)" $ do 
            parse (topLevel ['f']) "f(x,y)" `shouldBe` [(T "f" [V ('x',1),V ('y',1)], "")] 
        
        it "should parse a string representing the term f(f(a,b),x)" $ do 
            parse (topLevel ['f', 'a', 'b']) "f(f(a,b),x)" `shouldBe` [(T "f" [T "f" [T "a" [], T "b" []], V ('x',1)], "")]

        it "should parse a string representing the term x" $ do 
            parse (topLevel []) "x" `shouldBe` [(V ('x',1), "")]

        {-it "should parse a string representing the term xor(x,y)" $ do 
            parse (topLevel ["xor"]) "xor(x,y)" `shouldBe` [(T "xor" [V ('x',1), V ('y', 1)])] -}

        it "should parse a string representation with spaces of the term    f  (     x,    y  )   " $ do 
            parse (topLevel ['f']) "   f  (     x,    y  )   " `shouldBe` [(T "f" [V ('x',1), V ('y', 1)], "")] 