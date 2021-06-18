module Terms.Terms (
    FSym,
    OrderedSig,
    VName,
    Term (V, T),
    occurs,
    root,
    subterms,
    alphaConvert,
    maxIndex
) where

import Data.List ( intercalate )

type FSym       = String 
type OrderedSig = [FSym]
type VName = (Char, Int)

data Term 
    = V VName 
    | T String [Term] 
    deriving (Eq)

instance Show Term where 
    show (V x) = [fst x] 
    show t     = root t ++ "(" ++ intercalate "," (map show (subterms t)) ++ ")" 

occurs :: VName -> Term -> Bool 
occurs x (V y) = x == y
occurs x (T _ ts) = any (occurs x) ts 

root :: Term -> FSym 
root (T f ts) = f 

subterms :: Term -> [Term]
subterms (T f ts) = ts 

alphaConvert :: Int -> Term -> Term 
alphaConvert n (V (c,i)) = V (c,i+n) 
alphaConvert n (T f ts)  = T f (map (alphaConvert n) ts)

maxIndex :: Term -> Int
maxIndex (V (x,i)) = i 
maxIndex (T _ ts)  = maxs (map maxIndex ts)
    where maxs = foldr max 0
