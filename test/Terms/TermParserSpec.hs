module Terms.TermParserSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Terms.TermParser 
import Terms.Terms 

main :: IO ()
main = hspec $ do 
    describe "The term parser topLevel function" $ do 
        it "should parse a string representing a term" $ do 
            parse (topLevel ['f']) "f(x,y)" `shouldBe` [(T "f" [V ('x',1),V ('y',1)],"")] 