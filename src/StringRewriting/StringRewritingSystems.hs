{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

module StringRewriting.StringRewritingSystems (
      StringRewriteRule(..)
    , Alphabet
) where

data StringRewriteRule a b where 
    (:->:) :: a -> b -> StringRewriteRule a b
    deriving (Eq)

type Alphabet = [String]

