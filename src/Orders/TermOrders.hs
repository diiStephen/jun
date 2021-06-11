module Orders.TermOrders where 

import Orders.PolyOrders    
import Terms.Terms 
import Data.List (elemIndex, any, all)
import Control.Monad.Reader (Reader(..), ask, asks, runReader)

lpo :: OrderedSig -> Term -> Term -> Order 
lpo _ s (V x) = if occurs x s then GR else NGE
lpo _ (V x) _ = NGE 
lpo sig s t | any (\si -> lpo sig si t == GR || si == t) (subterms s) = GR
            | rootComp == GR && all (\tj -> lpo sig s tj == GR) (subterms t) = GR
            | rootComp == E && all (\tj -> lpo sig s tj == GR) (subterms t) && lexOrd (lpo sig) (subterms s) (subterms t) == GR = GR 
            | otherwise = NGE 
            where 
                rootComp = sym sig (root s) (root t)

mLpo :: Term -> Term -> Reader OrderedSig Order 
mLpo s (V x) = return $ if s == (V x) then E 
               else if occurs x s then GR else NGE 
mLpo (V _) (T _ _) = return $ NGE 
mLpo s t = do 
    rootComp <- asks (mSym (root s) (root t))
    sig <- ask 
    if any (\si -> runReader (mLpo si t) sig == GR) (subterms s) then 
        return GR 
    else 
        return $ case rootComp of 
            GR -> if all (\tj -> runReader (mLpo s tj) sig == GR) (subterms t) 
                  then GR else NGE 

            E -> if all (\tj -> runReader (mLpo s tj) sig == GR) (subterms t) 
                 then NGE --need to call lexOrd with mLpo here 
                 else NGE

            NGE -> NGE 


rpo :: OrderedSig -> Term -> Term -> Order 
rpo = undefined 

mSym :: FSym -> FSym -> OrderedSig -> Order 
mSym f g sig | f == g = E 
             | fIndex < gIndex = NGE 
             | otherwise = GR 
             where 
                 fIndex = elemIndex f sig 
                 gIndex = elemIndex g sig 

sym :: OrderedSig -> FSym -> FSym -> Order 
sym sig f g | f == g = E 
            | fIndex < gIndex = NGE 
            | otherwise = GR 
            where fIndex = elemIndex f sig  
                  gIndex = elemIndex g sig 
