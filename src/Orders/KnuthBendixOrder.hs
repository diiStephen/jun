module Orders.KnuthBendixOrder (
      kbo
    , isUnaryPower
) where

import Terms.Terms                ( Term(..), OrderedSig, FSym, subterms, isVar, root, occurs, collectAndCountVars )
import Orders.PolyOrders          ( Order(..), lexOrd )
import Orders.RecursivePathOrders ( sym )
import Data.List                  ( all )

kbo :: OrderedSig -> (Term -> Int) -> Term -> Term -> Order
kbo symOrder weight s t | varMeasure s t && weight s > weight t = GR
                        | varMeasure s t && weight s == weight t = case kbo2a s t of 
                            GR -> GR 
                            _ -> case kbo2bc symOrder weight s t of 
                                GR -> GR 
                                _ -> NGE
                        | otherwise = NGE 

-- Returns True if |s|_x >= |t|_x \forall x \in V
-- Assumes the return of collectAndCountVars is sorted
varMeasure :: Term -> Term -> Bool
varMeasure s t = go (collectAndCountVars s) (collectAndCountVars t)
    where
        go [] [] = True 
        go (_:_) [] = True 
        go [] (_:_) = False 
        go (vs:vss) (vt:vts) | fst vs == fst vt = (snd vs >= snd vt) && go vss vts
                             | otherwise = go vss (vt:vts) 

isUnaryPower :: FSym -> Term-> Bool
isUnaryPower _ (V _) = True
isUnaryPower g (T f ts) = f == g && (length ts == 1) && all (isUnaryPower f) ts

kbo2a :: Term -> Term -> Order  
kbo2a s (V x) | isUnaryPower (root s) s && occurs x s = GR 
              | otherwise = NGE
kbo2a s (T _ _) = NGE

kbo2bc :: OrderedSig -> (Term -> Int) -> Term -> Term -> Order 
kbo2bc symOrder weight (T f ss) (T g ts) = case sym symOrder f g of 
    GR -> GR 
    E -> lexOrd (kbo symOrder weight) ss ts 
    NGE -> NGE
kbo2bc _ _ _ _ = NGE
