{-# LANGUAGE GADTs, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module StringRewriting.StringRewritingSystems (
      StringRewriteRule(..)
    , Alphabet
    , StringRewriteSystem
    , stringToTerm
    , stringToTerm2
    , rewrite
    , rewriteAt
    , lefts
    , rights 
) where

import Terms.Terms    ( Term(..) )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Kind      ( Type )

type StringRewriteRule :: Type -> Type -> Type
data StringRewriteRule a b where 
    (:->:) :: a -> b -> StringRewriteRule a b
    deriving (Eq)

instance Bifunctor StringRewriteRule where 
    bimap f g (s :->: t) = f s :->: g t

type Alphabet = [String]

type StringRewriteSystem a b = [StringRewriteRule a b]

stringToTerm :: String -> Term 
stringToTerm s = go (reverse s) 
    where 
        go [] = V ('x', 1)
        go (c:cs) = T [c] [go cs]

stringToTerm2 :: Foldable t => t Char -> Term
stringToTerm2 = foldl (\t c -> T [c] [t]) (V ('x',1))

rewrite :: StringRewriteRule String String -> String -> String
rewrite (l :->: r) s = if l == s then r else s

rewriteAt :: StringRewriteRule String String -> String -> Int -> String
rewriteAt (l :->: r) s p = before ++ rewrite (l :->: r) redex ++ after
    where 
        (before, suffix) = splitAt (p-1) s
        (redex, after) = splitAt (length l) suffix

lefts :: StringRewriteSystem a b -> [a]
lefts = map lhs

lhs :: StringRewriteRule a b -> a 
lhs (l :->: _) = l

rights :: StringRewriteSystem a b -> [b]
rights = map rhs

rhs :: StringRewriteRule a b -> b 
rhs (_ :->: r) = r