module Tile where

type Tiles = [Tile]

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
    | num > 9 || num < 1 = error "Invalid conversion"
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
isTerminal (Number _ One) = True
isTerminal (Number _ Nine) = True
isTerminal _ = False


areSameSuit :: Tiles -> Bool
areSameSuit [] = True
areSameSuit (Number s _) : xs = all (samesuit s)
  where
    samesuit s (Number s' _) = s == s'
    samesuit _ = False

areEqual :: Tiles -> Bool
areEqual [] = True
areEqual x : xs = all (== x) xs


areConsecutive :: Tiles -> Bool
areConsecutive [] = True
areConsecutive xs = isSameSuit xs && or do
  nums <- sequence $ unwrap xs
  return $ and $ mapAdj consec (sort nums)
  where
    unwrapNumber Number _ n = Just n
    unwrapNumber _ = None
    unwrap = fmap unwrapNumber 

consec :: Number -> Number -> Bool
consec n1 n2   
  | n1 == n2 = False
  | n1 == Nine = (n1 == succ n2) 
  | n2 == Nine = (n2 == succ n1) 
  | _  = (n1 == succ n2) || (n2 == succ n1) 

normFive :: Number -> Number
normFive RFive = Five