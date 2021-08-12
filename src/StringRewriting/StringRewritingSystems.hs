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
    , rewrite3
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

--Rewrites strings of the form lw where l->r is the rule and w \in \Sigma^*
--This may not be the correct definition as redex are defined as strings of the form wl
rewrite2 :: StringRewriteRule String String -> String -> Maybe String 
rewrite2 (l :->: r) s = if take len s == l then Just (r ++ drop len s) else Nothing 
    where len = length l

-- Rewrites redexes of the form wl to wr 
rewrite3 :: StringRewriteRule String String -> String -> Maybe String 
rewrite3 _ [] = Nothing 
rewrite3 (l :->: r) s = if drop prefix s == l then Just (take prefix s ++ r) else Nothing 
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
rewriteAll (r:rs) s = case rewrite3 r s of  
    Just contr -> Just contr
    Nothing    -> rewriteAll rs s

{--- Currently incorrect because the entire suffix is attempted to be matched against the rule. 
normalize :: StringRewriteSystem String String -> String -> String 
normalize _ [] = []
normalize srs (s:ss) = let u = s : normalize srs ss in 
    case rewriteAll srs u of  
        Just contr -> normalize srs contr 
        Nothing    -> u
-}

normalize :: StringRewriteSystem String String -> String -> String 
normalize srs s = take (length u - 1) u
    where u = normalizer srs "" (s ++ "$")  

normalizer :: StringRewriteSystem String String -> String -> String -> String
normalizer _ prefix [] = prefix
normalizer srs prefix (t:ts) = case rewriteAll srs prefix of 
    Just s -> normalizer srs (normalizer srs [] s) (t:ts)
    Nothing -> normalizer srs (prefix ++ [t]) ts
