{-# LANGUAGE InstanceSigs #-}

module Terms.Terms2 where 

import Data.Kind ( Type )
import Data.List (any, intercalate)
import Data.Coerce (coerce)

type FSym       = String 
type OrderedSig = [FSym]

data VName 
    = VName {name :: String, index :: Int}
    deriving (Eq)

data Term a 
    = T { root :: FSym, subterms :: [Term a] }
    | V a
    | Empty
    deriving (Eq)

instance Show VName where 
    show = name

instance Show a => Show (Term a) where 
    show (V x) = show x 
    show t     = root t ++ "(" ++ intercalate "," (map show (subterms t)) ++ ")" 

instance Functor Term where 
    fmap :: (a -> b) -> Term a -> Term b 
    fmap f (V x) = V $ f x
    fmap f t     = T (root t) (fmap (fmap f) (subterms t))

instance Foldable Term where 
    foldMap :: Monoid m => (a -> m) -> Term a -> m
    foldMap _ Empty = mempty 
    foldMap f (V x) = f x
    foldMap f (T r ts) = mconcat (map (foldMap f) ts)
 
occurs :: Eq a => a -> Term a -> Bool 
occurs x (V y) = x == y 
occurs x t     = any (occurs x) (subterms t)
