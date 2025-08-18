module Tile where

data Tile 
  = Standard Suit Number
  | Wind Cardinal
  | Dragon DColor
  deriving (Eq, Show)

data Number 
  = One
  | Two
  | Three
  | Four
  | Five
  | RFive
  | Six
  | Seven
  | Eight
  | Nine 
  | Ten
  deriving (Eq, Show)

data Suit 
  = Manzu
  | Pinzu
  | Sozu
  deriving (Eq, Show)

data Cardinal 
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data DColor 
  = White
  | Red
  | Green
  deriving (Eq, Show)
  


 terminal ::  Tile -> Bool
 terminal Standard _ One = True
 terminal Standard _ Nine = True
 terminal _ = False
