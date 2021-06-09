{-# LANGUAGE InstanceSigs #-}

module Terms.Terms2 where 

type FSym       = String 
type VName      = (String, Int)
type OrderedSig = [FSym]

data Term a 
    = T { root :: FSym, subterms :: [Term a] }
    | V a
    deriving (Show, Eq)

instance Functor Term where 
    fmap :: (a -> b) -> Term a -> Term b 
    fmap f (V x) = V $ f x
    fmap f t     = T (root t) (fmap (fmap f) (subterms t))