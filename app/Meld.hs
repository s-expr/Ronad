module Meld where

import Tile
import Utils
import Data.List (sort)

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
  else None

-- Smart Contructors 
tryMakeChi :: Tiles -> Maybe Set
tryMakeChi = tryMake Chi 

tryMakePon :: Tiles -> Maybe Set
tryMakePon = tryMake Pon 

tryMakeKan :: Tiles -> Maybe Set
tryMakeKan = tryMake Kan 

makeMeld :: Set -> RevealState -> Maybe Meld
makeMeld = Meld 

isEqual :: Tiles -> Bool
isEqual [] = True
isEqual x : xs = all (== x) xs

isValid :: SetType -> (Tiles -> Bool)
isValid Chi = isConsecutive .&&. isLen 3 
isValid Pon = isEqual .&&. isLen 3 
isValid Kan = isEqual .&&. isLen 4 

getTiles :: Meld -> [Tile]
getTiles (Meld (Set _ tiles)) = tiles

isOpen :: Meld -> Bool
isOpen (Meld _ Open) = True
isOpen _ = False