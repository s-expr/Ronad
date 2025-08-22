module Utils where

import qualified Data.HashTable.ST.Cuckoo as C
import Data.HashTable


type HistPredicate s k v = (C.HashTable s k v) 
-- This is so pog :D POG CHAMP chat what do you think ? i think i did pog a little ;P ¯\_(ツ)_/¯ my mom beats me sometimes :( it has made me ve
isLen :: Int -> a -> Bool
isLen sz = (== sz) . length  

mapAdj :: (a -> a -> b) -> [a] -> [b]
mapAdj f lst = zipWith f (tail lst) lst 

find 
  :: (Hashtable h, Eq k) 
  => ((k, v) -> Bool) 
  -> h s k v 
  -> ST s (Maybe (k, v))
find f = foldM pred Nothing 
  where 
    pred Nothing kv = if f kv then Just kv else Nothinhg
    pred kv@Just _ = kv
    

hist :: (Foldable t,  Eq a) => t a -> ST s (C.HashTable s a Int)
tilesToMap tiles = foldM_ (\ht k -> mutate ht k inc) . newSized . length
    where 
      inc Just count = (Just count + 1, ())
      inc Nothing = (1, ())

-- Predicate combinators
(.&&.) f g = liftA2 (&&)
(.||.) f g = liftA2 (||)
