module Orders.TermOrders where 

import Orders.PolyOrders    
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

rpo :: OrderedSig -> Term -> Term -> Order 
rpo = undefined 

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
