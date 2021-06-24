module Terms.Terms (
    FSym,
    OrderedSig,
    VName,
    Term (V, T),
    occurs,
    root,
    subterms,
    alphaConvert,
    maxIndex,
    label,
    pos,
    fpos
) where

import Data.List ( intercalate, union )

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
subterms (V _)    = []

alphaConvert :: Int -> Term -> Term 
alphaConvert n (V (c,i)) = V (c,i+n) 
alphaConvert n (T f ts)  = T f (map (alphaConvert n) ts)

maxIndex :: Term -> Int
maxIndex (V (x,i)) = i 
maxIndex (T _ ts)  = maxs (map maxIndex ts)
    where maxs = foldr max 0

label :: Term -> [(Term, [Char])]
label t = [(t,"")] `union` foldr union [] (zipWith go prefixes ts)
    where 
        ts       = subterms t
        prefixes = map show (take (length ts) [1..])
        go s t   = [(u,s++x) | (u,x) <- label t]

pos :: Term -> [String]
pos t = [""] `union` foldr union [] (zipWith go prefixes ts)
    where 
        ts       = subterms t
        prefixes = map show (take (length ts) [1..])
        go :: String -> Term -> [String]
        go s t   = [s++x | x <- pos t]

fpos :: Term -> [Term]
fpos (V _)    = []
fpos (T f ts) = (:) (T f ts) (ts >>= fpos)