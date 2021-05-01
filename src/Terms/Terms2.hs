{-# LANGUAGE GADTs, StandaloneKindSignatures, DataKinds #-}

module Terms.Terms2 where 

import Data.Kind

data Nat = Zero | S Nat  

type GFunSym :: Nat -> Type 
data GFunSym n where 
    GF :: String -> Nat -> GFunSym n

data FunSym     = F String Int deriving (Show)
data VName      = Vn String Int
data Term       = V VName | T FunSym [Term] 
data Signature  = Sig [FunSym] deriving (Show)
data Equation   = E Term Term 
data Theory     = Th [Equation]

newtype TermParser a = P (String -> [(a, String)])

mkFunSyms :: [(String, Int)] -> [FunSym]
mkFunSyms []         = []
mkFunSyms ((f,a):fs) = (F f a) : (mkFunSyms fs)

sig :: [(String,Int)] -> Signature
sig = Sig . mkFunSyms

arity :: FunSym -> Int 
arity (F _ a) = a 