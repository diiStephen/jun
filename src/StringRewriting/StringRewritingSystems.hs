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
    , rewriteAll
    , normalize
    , reduce
) where

import Terms.Terms    ( Term(..) )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Kind      ( Type )
import Data.Maybe     ( fromMaybe )

--This type should be generalized to RewriteRule and specialized to String and Term rewriting systems. 
type StringRewriteRule :: Type -> Type -> Type
data StringRewriteRule a b where 
    (:->:) :: a -> b -> StringRewriteRule a b
    deriving (Eq, Show)

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

rewrite :: StringRewriteRule String String -> String -> Maybe String
rewrite (l :->: r) s = if l == s then Just r else Nothing

-- Rewrites redexes of the form wl to wr 
reduce :: StringRewriteRule String String -> String -> Maybe String 
reduce _ [] = Nothing 
reduce (l :->: r) s = if drop prefix s == l then Just (take prefix s ++ r) else Nothing 
    where prefix = length s - length l

rewriteAt :: StringRewriteRule String String -> String -> Int -> String
rewriteAt (l :->: r) s p = before ++ fromMaybe redex (rewrite (l :->: r) redex) ++ after
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

rewriteAll :: StringRewriteSystem String String -> String -> Maybe String
rewriteAll [] _ = Nothing 
rewriteAll (r:rs) s = case reduce r s of  
    Just contr -> Just contr
    Nothing    -> rewriteAll rs s

-- Needs to add an end marker, $, at the end of string. 
normalize :: StringRewriteSystem String String -> String -> String 
normalize srs s = take (length u - 1) u
    where u = normalizer srs "" (s ++ "$")  

normalizer :: StringRewriteSystem String String -> String -> String -> String
normalizer _ prefix [] = prefix
normalizer srs prefix (t:ts) = case rewriteAll srs prefix of 
    Just s  -> normalizer srs (normalizer srs [] s) (t:ts)
    Nothing -> normalizer srs (prefix ++ [t]) ts
