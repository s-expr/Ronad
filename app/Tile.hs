module Tile where

data Tile 
  = Number Suit Number
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
  deriving (Eq, Ord, Show)

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
 
instance Enum Number where
  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4
  fromEnum Five = 5
  fromEnum RFive = 5
  fromEnum Six = 6
  fromEnum Seven = 7
  fromEnum Eight = 8
  fromEnum Nine = 9

  toEnum num 
    | num > 9 || num < 1= error "Invalid conversion"
  toEnum 1 = One 
  toEnum 2 = Two  
  toEnum 3 = Three  
  toEnum 4 = Four  
  toEnum 5 = Five  
  toEnum 6 = Six  
  toEnum 7 = Seven  
  toEnum 8 = Eight  
  toEnum 9 = Nine  
 
instance Ord Number where  
  compare n1 n2 = compare (fromEnum n1) (fromEnum n2)




isTerminal :: Tile -> Bool 
isTerminal Number _ One = True
isTerminal Number _ Nine = True
isTerminal _ = False

normFive :: Number -> Number
normFive RFive = Five