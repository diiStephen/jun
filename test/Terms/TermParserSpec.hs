module Terms.TermParserSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do 
    describe "Prelude.head" $ do 
        it "returns the first element of a list" $ do 
            head [23,24] `shouldBe` (23 :: Int)
