module TermRewriting.Rewrite where 

import Terms.Terms 
import Unification.Unification 
import Substitution.Substitutions
import Control.Applicative

{-Feels like this should be a 1 liner-}
rewrite :: (Term, Term) -> Term -> Maybe Term
rewrite (l,r) s = case match' l s of 
                Just sigma -> Just (applyLifted sigma r)
                Nothing    -> Nothing 

{-Handles the failure case as bind is defined according to Just a / Nothing -}
rewrite2 :: (Term, Term) -> Term -> Maybe Term 
rewrite2 (l,r) s = do 
    sigma <- match' l s 
    return (applyLifted sigma r)  

{-Removing the sugar-}
rewrite3 :: (Term, Term) -> Term -> Maybe Term 
rewrite3 (l,r) s = match' l s >>= \sigma -> return (applyLifted sigma r)

{-Monad free!-}
rewrite4 :: (Term, Term) -> Term -> Maybe Term 
rewrite4 (l,r) s = pure applyLifted <*> (match' l s) <*> pure r

rewriteAll :: [(Term, Term)] -> Term -> Maybe Term 
rewriteAll [] _ = Nothing
rewriteAll (r:rs) s = case rewrite r s of
                  Just t  -> Just t 
                  Nothing -> rewriteAll rs s

{-Potentially non-terminating-} 
normalize :: [(Term, Term)] -> Term -> Term 
normalize _ (V x) = V x
normalize trs (T f ts) = let u = (T f (map (normalize trs) ts)) in 
                               case rewriteAll trs u of 
                                    Just s  -> normalize trs s 
                                    Nothing -> u

{-Potentially non-terminating-}
normalize2 :: [(Term,Term)] -> Term -> Term 
normalize2 trs = go 
    where 
        go :: Term -> Term 
        go (V x) = V x 
        go (T f ts) = 
            let u = (T f (map go ts)) in 
                case rewriteAll trs u of 
                    Just s  -> go s 
                    Nothing -> u 
