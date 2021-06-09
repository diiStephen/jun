module Orders.TermOrders where 

import Orders.PolyOrders    
import Terms.Terms 
import Data.List (elemIndex)

type FSym       = String 
type OrderedSig = [FSym]

lpo :: OrderedSig -> Term -> Term -> Order 
lpo = undefined 

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
