module Orders.TermOrders where 

import Orders.PolyOrders    
import Terms.Terms 
import Data.List (elemIndex, any, all)

lpo :: OrderedSig -> Term -> Term -> Order 
lpo _ s (V x) = if occurs x s then GR else NGE
lpo _ (V x) _ = NGE 
lpo sig s t | any (\si -> lpo sig si t == GR || si == t) (subterms s) = GR
            | rootComp == GR && all (\tj -> lpo sig s tj == GR) (subterms t) = GR
            | rootComp == E && all (\tj -> lpo sig s tj == GR) (subterms t) && lexOrd (lpo sig) (subterms s) (subterms t) == GR = GR 
            | otherwise = NGE 
            where 
                rootComp = sym sig (root s) (root t)

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
