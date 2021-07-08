module TermRewriting.Rewrite (
    RewriteRule (..),
    RewriteSystem (..),
    normalize,
    mkDisjointVars,
    basic,
    addRule
) where 

import Terms.Terms                ( Term(..), alphaConvert, maxIndex ) 
import Unification.Unification    ( match' )
import Substitution.Substitutions ( applyLifted )
import Control.Applicative        ()
import Data.List                  ( intercalate )

data RewriteRule          = Rule { lhs :: Term, rhs :: Term }
newtype RewriteSystem     = Rules { rules :: [RewriteRule] }

instance Show RewriteRule where 
    show r = show (lhs r) ++ " --> " ++ show (rhs r)

instance Show RewriteSystem where 
    show rs = "{" ++ intercalate " , " (map show (rules rs)) ++ "}" 

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
normalize trs (T f ts) = let u = T f (map (normalize trs) ts) in 
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
            let u = T f (map go ts) in 
                case rewriteAll trs u of 
                    Just s  -> go s 
                    Nothing -> u 

mkDisjointVars :: RewriteRule -> RewriteRule -> RewriteRule
mkDisjointVars rho tau = Rule variantLhs variantRhs
    where
        variantLhs = alphaConvert offset (lhs rho)
        variantRhs = alphaConvert offset (rhs rho)
        offset = max (maxIndex (lhs tau)) (maxIndex (rhs tau)) + 1

basic :: RewriteSystem -> [(Term,Term)]
basic trs = map (\r -> (lhs r, rhs r)) (rules trs)

addRule :: RewriteSystem -> RewriteRule -> RewriteSystem 
addRule trs r = Rules $ r:rules trs   