module Orders.TermOrders where 

import Orders.PolyOrders    
import Terms.Terms 
import Data.List (elemIndex, any, all)

type FSym       = String 
type OrderedSig = [FSym]

lpo :: OrderedSig -> Term -> Term -> Order 
lpo sig s (V x) = if occurs x s then GR else NGE 
lpo sig (T f ss) (T g ts) | any (\si -> (lpo sig si (T g ts) == GR) || (si == (T g ts))) ss = GR
                          | (sym sig f g == GR) && (all (\tj -> lpo sig (T f ss) tj == GR) ts) = GR 
                          | (sym sig f g == E) && (all (\tj -> lpo sig (T f ss) tj == GR) ts) && ((lexOrd (lpo sig) ss ts) == GR) = GR 
                          | otherwise = NGE 
lpo sig (V x) _ = NGE 

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
