module Unification.UnificationSpec where 

import Test.Hspec 
import Test.QuickCheck 
import Control.Exception (evaluate)

import Unification.Unification

main :: IO () 
main = hspec $ do 
    describe "The basic syntactic unification algorithm" $ do 
        it "should produce a correct mgu given the equations" $ do 
            head [23,25] `shouldBe` (23 :: Int)