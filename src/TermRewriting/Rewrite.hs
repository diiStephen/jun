module TermRewriting.Rewrite (
    RewriteRule (..),
    RewriteSystem (..),
    rewrite,
    rewriteAll,
    normalize,
    mkDisjointVars,
    addRule,
    mkRewriteSystem
) where 

import Terms.Terms                ( Term(..), alphaConvert, maxIndex ) 
import Unification.Unification    ( match' )
import Substitution.Substitutions ( applyLifted )
import Control.Applicative        ()
import Data.List                  ( intercalate )
import Data.Coerce                ( coerce )

data RewriteRule      = Rule { lhs :: Term, rhs :: Term }
newtype RewriteSystem = Rules { rules :: [RewriteRule] }

instance Show RewriteRule where 
    show r = show (lhs r) ++ " --> " ++ show (rhs r)

instance Show RewriteSystem where 
    show rs = "{ " ++ intercalate " , " (map show (rules rs)) ++ " }" 

mkRewriteSystem :: [RewriteRule] -> RewriteSystem
mkRewriteSystem = coerce

addRule :: RewriteSystem -> RewriteRule -> RewriteSystem 
addRule trs r = Rules $ r:rules trs  

rewrite :: RewriteRule -> Term -> Maybe Term 
rewrite (Rule l r) s = (applyLifted <$> match' l s) <*> pure r

rewriteAll :: [RewriteRule] -> Term -> Maybe Term 
rewriteAll [] _ = Nothing
rewriteAll (r:rs) s = case rewrite r s of
    Just t  -> Just t 
    Nothing -> rewriteAll rs s

normalize :: RewriteSystem -> Term -> Term 
normalize _ (V x) = V x
normalize trs (T f ts) = let u = T f (map (normalize trs) ts) in 
    case rewriteAll (rules trs) u of 
        Just s  -> normalize trs s 
        Nothing -> u

mkDisjointVars :: RewriteRule -> RewriteRule -> RewriteRule
mkDisjointVars rho tau = Rule variantLhs variantRhs
    where
        variantLhs = alphaConvert offset (lhs rho)
        variantRhs = alphaConvert offset (rhs rho)
        offset = max (maxIndex (lhs tau)) (maxIndex (rhs tau)) + 1 