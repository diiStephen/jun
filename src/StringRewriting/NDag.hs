{-# OPTIONS_GHC -Wall #-}

module StringRewriting.NDag (
      delta
    , gamma
    , suf
) where

import StringRewriting.StringRewritingSystems ( StringRewriteSystem, Alphabet )
import Data.List                              ( tails )

suf :: String -> [String]
suf = tails 

gamma :: StringRewriteSystem String String -> [String]
gamma = undefined 

delta :: Alphabet -> [String]
delta = undefined