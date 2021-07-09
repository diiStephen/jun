module Orders.MonadicRecursivePathOrders (
    mLpo
) where 

import Orders.PolyOrders    ( Order(..) )
import Terms.Terms          ( Term(..), OrderedSig, FSym, occurs, root, subterms ) 
import Data.List            ( elemIndex, any, all, find )
import Control.Monad.Reader ( Reader(..), ask, asks, runReader, zipWithM )

mLpo :: Term -> Term -> Reader OrderedSig Order

mLpo s (V x) = return $ if s == V x then E 
               else if occurs x s then GR else NGE 

mLpo (V _) (T _ _) = return NGE 

mLpo s t = do 
    terms <- mapM (`mLpo` t) (subterms s)
    if GR `elem` terms then 
        return GR 
    else
        do
            symPrec <- asks (mSym (root s) (root t))
            domSet <- mapM (mLpo s) (subterms t)
            case symPrec of 
                GR -> if all (== GR) domSet 
                      then return GR else return NGE 

                E -> if all (== GR) domSet 
                     then do
                         lexExt <- zipWithM mLpo (subterms s) (subterms t)
                         case find (/= E) lexExt of 
                             Just r  -> return r 
                             Nothing -> return E  
                     else return NGE

                NGE -> return NGE 

mSym :: FSym -> FSym -> OrderedSig -> Order 
mSym f g sig | f == g = E 
             | fIndex < gIndex = NGE 
             | otherwise = GR 
             where 
                 fIndex = elemIndex f sig 
                 gIndex = elemIndex g sig 