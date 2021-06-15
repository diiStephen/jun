module Orders.TermOrders (
    rpo,
    lpo, 
    mpo,    
) where 

import Orders.PolyOrders ( Order(..), lexOrd, multiOrder ) 
import Terms.Terms       ( Term(..), OrderedSig, FSym, occurs, root, subterms ) 
import Data.List         ( elemIndex, any, all )
 
rpo :: [FSym] -> (FSym -> (Term -> Term -> Order) -> [Term] -> [Term] -> Order) -> Term -> Term -> Order
rpo _ _ s (V x) | s == V x = E 
                | occurs x s = GR 
                | otherwise = NGE 
      
rpo _ _ (V _) (T _ _) = NGE 
rpo sig stat s t = if any (\si -> rpo sig stat si t == GR || si == t) (subterms s)
                   then GR 
                   else case rootComp of 

                       GR -> if all (\tj -> rpo sig stat s tj == GR) (subterms t)
                             then GR else NGE 

                       E ->  if all (\tj -> rpo sig stat s tj == GR) (subterms t)
                             then stat (root s) (rpo sig stat) (subterms s) (subterms t)
                             else NGE 
                        
                       NGE -> NGE 
                where 
                    rootComp = sym sig (root s) (root t)

lpo :: OrderedSig -> Term -> Term -> Order 
lpo sig = rpo sig (const lexOrd)

mpo :: OrderedSig  -> Term -> Term -> Order 
mpo sig = rpo sig (const multiOrder)

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
