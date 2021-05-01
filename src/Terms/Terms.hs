module Terms.Terms (
    VName,
    Term (V, T),
    occurs
) where

type Sig = [String]

type VName = (Char, Int)

data Term 
    = V VName 
    | T String [Term] 
    deriving (Eq,Show)

occurs :: VName -> Term -> Bool 
occurs x (V y) = x == y
occurs x (T _ ts) = any (occurs x) ts 
