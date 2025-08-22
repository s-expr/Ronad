module Hand where

import Tile
import Meld

data Wait
  = Ryanmen -- Missing edges (e.g., 23 waiting for 1 or 4)
  | Kanchan -- Missing middle (e.g., 13 waiting for 2) 
  | Penchan -- Missing edge (e.g., 12 waiting for 3)
  | Shanpon -- Missing triplet (e.g., 22 waiting for 2)
  | Tanki -- Missing pair (e.g., 2 waiting for 2)
  deriving (Show, Eq, Ord, Enum, Bounded)

data Hand = Hand
  { closed :: Tiles
  , melds :: [Tiles]
  }

plays :: Tile -> Hand -> Maybe [Meld] 
plays = findMelds . closed

totalTiles :: Hand -> Int
totalTiles hand = length (closed hand) + sum (map length (melds hand))

isOpen :: Hand -> Bool
isOpen hand = any isOpen (melds hand)

isTenpai :: Hand -> Bool
isTenpai hand = and $ map (isInWait hand) allwaits
    where
      allwaits = [minBound .. maxBound]

isInWait :: Hand -> Wait -> Bool
isInWait hand Ryanmen = _
isInWait hand Kanchan = _
isInWait hand Penchan = _
isInWait hand Shanpon  = _
isInWait hand Tanki  = _

isComplete :: Hand -> Bool
isComplete hand = totalTiles hand == 14 && hasValidWinningHand hand

hasValidWinningHand :: Hand -> Bool
hasValidWinningHand hand =
  standardWin (closed hand) (length (melds hand))

standardWin :: Tiles -> Int -> Bool
standardWin closedTiles meldCount = 
  let meldsNeeded = 4 - meldCount
    tilesNeeded = meldsNeeded * 3 + 2
  in length closedTiles == tilesNeeded &&
    canWinWithTiles closedTiles meldsNeeded

canWinWithTiles :: Tiles -> Int -> Bool
canWinWithTiles [t1, t2] 0 = t1 == t2
canWinWithTiles _ 0 = False
canWinWithTiles tiles meldsNeeded = _
