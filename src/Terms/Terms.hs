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
    fpos,
    get,
    set,
    isNonVar,
    isVar,
    size,
    collectVars,
    collectAndCountVars
) where

import Data.List ( intercalate, union )
import qualified Data.Map as Map

type FSym       = String 
type OrderedSig = [FSym]
type VName = (Char, Int)

-- NB: Term derives Ord, but this is NOT a meaningful term ordering. It is used 
-- only for the purposes of containers like Sets, HashMap, etc which impose a 
-- Ord typeclass constraint. 
data Term 
    = V VName 
    | T String [Term] 
    deriving (Eq, Ord)

instance Show Term where 
    show (V x) = fst x : show (snd x) 
    show t     = if null (subterms t) then root t
                 else root t ++ "(" ++ intercalate "," (map show (subterms t)) ++ ")" 

occurs :: VName -> Term -> Bool 
occurs x (V y) = x == y
occurs x (T _ ts) = any (occurs x) ts 

root :: Term -> FSym 
root (T f ts) = f 
root (V x)    = [fst x]

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

label :: Term -> [(Term, String)]
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
        go s t   = [s++x | x <- pos t]

fpos :: Term -> [Term]
fpos (V _)    = []
fpos (T f ts) = (:) (T f ts) (ts >>= fpos)

get :: Term -> String -> Term 
get = foldl (\ t s -> (!!) (subterms t) (read [s] - 1))

-- t[s]_p
set :: Term -> Term -> String -> Term
set t s "" = s 
set t s (p:ps) = T (root t) (init (fst spl)++[set (last $ fst spl) s ps]++snd spl)
    where spl = splitAt (read [p]) (subterms t)

isNonVar :: Term -> Bool
isNonVar (V _) = False 
isNonVar (T _ _) = True

isVar :: Term -> Bool
isVar  = not . isNonVar 

size :: Term -> Int 
size (V x) = 1 
size (T f ts) = 1 + sum (map size ts)

collectVars :: Term -> [VName]
collectVars = collectVars' []
    where
        collectVars' :: [VName] -> Term -> [VName]
        collectVars' vs (V x) = x:vs
        collectVars' vs (T f ts) = concatMap (collectVars' vs) ts

collectAndCountVars :: Term -> [(VName, Int)]
collectAndCountVars t = Map.toAscList (collectAndCountVars' Map.empty t)
    where
        collectAndCountVars' :: Map.Map VName Int -> Term -> Map.Map VName Int 
        collectAndCountVars' m (V x) = Map.insertWith (+) x 1 m
        collectAndCountVars' m (T f ts) = foldl collectAndCountVars' m ts