module Meld where

import Tile
import Utils
import Data.List (sort)
import qualified Data.HashTable.ST.Cuckoo as C
import Data.HashTable



type TileHist s = C.HashTable s Tile Int

data Set = Set {type :: SetType , tiles :: Tiles} 
  deriving (Eq, Show)

data SetType
  = Chi 
  | Pon
  | Kan 
  deriving (Eq, Show)

data RevealState
  = Open
  | Closed
  deriving (Eq, Show)
  
data Meld = Meld Set RevealState
  deriving (Eq, Show)

tryMake :: SetType -> Tiles -> MaybeSet
tryMake type tiles =
  if isValid type tiles 
  then Just Set type tiles 
  else Nothing

-- Smart Contructors 
tryMakeChi :: Tiles -> Maybe Set
tryMakeChi = tryMake Chi 

tryMakePon :: Tiles -> Maybe Set
tryMakePon = tryMake Pon 

tryMakeKan :: Tiles -> Maybe Set
tryMakeKan = tryMake Kan 

makeMeld :: Set -> RevealState -> Maybe Meld
makeMeld = Meld 

isValid :: SetType -> (Tiles -> Bool)
isValid Chi = areConsecutive .&&. isLen 3 
isValid Pon = areEqual .&&. isLen 3 
isValid Kan = areEqual .&&. isLen 4 

getTiles :: Meld -> [Tile]
getTiles (Meld (Set _ tiles)) = tiles

isOpen :: Meld -> Bool
isOpen (Meld _ Open) = True
isOpen _ = False

hasSet :: (Hashtable h, Eq k) => h s k v -> SetType -> ST s Bool
hasSet set = isJust .  case set of 
  Chi -> undefined
  Pon -> find minCount 3
  Kan -> find minCount 4
    where
      minCount n (tile, count) = count >= n

hasAnySets :: Tiles -> [SetType] -> Bool
hasAnySets tiles sets = runST $ do
  tilesHist <- hist tiles
  let hasFoundAny = map (hasSet ht) sets 
  return $ or hasFoundAny