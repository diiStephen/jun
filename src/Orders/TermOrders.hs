module Orders.TermOrders (
    lpo, 
    rpo  
) where 

import Orders.PolyOrders (Order(..), lexOrd) 
import Terms.Terms 
import Data.List (elemIndex, any, all)
 
lpo :: OrderedSig -> Term -> Term -> Order 
lpo _ s (V x) = if s == (V x) then E 
                else if occurs x s then GR else NGE
lpo _ (V _) (T _ _) = NGE 
lpo sig s t = if any (\si -> lpo sig si t == GR || si == t) (subterms s) 
              then GR 
              else case rootComp of
                  
                  GR -> if all (\tj -> lpo sig s tj == GR) (subterms t)
                        then GR else NGE   
                  
                  E -> if all (\tj -> lpo sig s tj == GR) (subterms t) 
                       then lexOrd (lpo sig) (subterms s) (subterms t)
                       else NGE

                  NGE -> NGE 
            where 
                rootComp = sym sig (root s) (root t)

--rpo :: OrderedSig -> (FSym -> ([a] -> [a] -> Order) -> [a] -> [a] -> Order) -> Term -> Term -> Order 
rpo _ _ s (V x) = if s == (V x) then E 
                  else if occurs x s then GR else NGE 
rpo _ _ (V _) (T _ _) = NGE 
rpo sig stat s t = if any (\si -> rpo sig stat si t == GR || si == t) (subterms s)
                   then GR 
                   else case rootComp of 

                       GR -> if all (\tj -> rpo sig stat s tj == GR) (subterms t)
                             then GR else NGE 

                       E ->  if all (\tj -> rpo sig stat s tj == GR) (subterms t)
                             then (stat (root s)) (rpo sig stat) (subterms s) (subterms t)
                             else NGE 
                        
                       NGE -> NGE 
                where 
                    rootComp = sym sig (root s) (root t)

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
