module Utils.MonadUtils (
      mapMaybeM
    , commute
) where

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as = mapMaybeM' f as [] 
    where 
        mapMaybeM' _ [] acc = return acc 
        mapMaybeM' f (a:as) acc = do 
            fa <- f a 
            case fa of 
                Nothing -> mapMaybeM' f as acc 
                Just x -> mapMaybeM' f as (x:acc) 

commute :: (a, Maybe b) -> Maybe (a, b)
commute (i, r) = case r of 
    Just rule -> Just (i, rule) 
    Nothing -> Nothing