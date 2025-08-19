module Utils where

isLen :: Int -> a -> Bool
isLen sz = (== sz) . length  

mapAdj :: (a -> a -> b) -> [a] -> [b]
mapAdj f lst = zipWith f (tail lst) lst 

-- Predicate combinators
(.&&.) f g = liftA2 (&&)
(.||.) f g = liftA2 (||)
