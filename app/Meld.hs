module Meld where

import Tile
import Data.List (sort)

data Set
  = Chi [Tile]
  | Pon [Tile]
  | OpenKan [Tile]
  | ClosedKan [Tile]
  deriving (Eq, Show)

data Meld = Meld Set
  deriving (Eq, Show)



makeMeld :: Set -> Maybe Meld
makeMeld (Chi tiles)
  | isValidChi tiles = Just (Meld (Chi tiles))
makeMeld (Pon tiles)
  | isValidPon tiles = Just (Meld (Pon tiles))
makeMeld (OpenKan tiles)
  | isValidKan tiles = Just (Meld (OpenKan tiles))
makeMeld (ClosedKan tiles)
  | isValidKan tiles = Just (Meld (ClosedKan tiles))
makeMeld _ = Nothing

normFive :: Number -> Number
normFive RFive = Five
normFive n = n

isValidChi :: [Tile] -> Bool
isValidChi tiles
  | length tiles /= 3 = False
isValidChi [Standard s1 n1, Standard s2 n2, Standard s3 n3] =
  s1 == s2 && s2 == s3 && isConsecutive [n1, n2, n3]
  where
    isConsecutive nums = 
      let sorted = sort (map normFive nums)
      in case sorted of
        [One, Two, Three] -> True
        [Two, Three, Four] -> True
        [Three, Four, Five] -> True
        [Four, Five, Six] -> True
        [Five, Six, Seven] -> True
        [Six, Seven, Eight] -> True
        [Seven, Eight, Nine] -> True
        _ -> False
isValidChi _ = False

isValidPon :: [Tile] -> Bool
isValidPon tiles
  | length tiles /= 3 = False
isValidPon [Standard s1 n1, Standard s2 n2, Standard s3 n3] =
  s1 == s2 && s2 == s3 && normFive n1 == normFive n2 && normFive n2 == normFive n3
isValidPon [t1, t2, t3] = t1 == t2 && t2 == t3
isValidPon _ = False

isValidKan :: [Tile] -> Bool
isValidKan tiles
  | length tiles /= 4 = False
isValidKan [Standard s1 n1, Standard s2 n2, Standard s3 n3, Standard s4 n4] =
  s1 == s2 && s2 == s3 && s3 == s4 && 
  normFive n1 == normFive n2 && normFive n2 == normFive n3 && normFive n3 == normFive n4
isValidKan [t1, t2, t3, t4] = t1 == t2 && t2 == t3 && t3 == t4
isValidKan _ = False


getTiles :: Meld -> [Tile]
getTiles (Meld (Chi tiles)) = tiles
getTiles (Meld (Pon tiles)) = tiles
getTiles (Meld (OpenKan tiles)) = tiles
getTiles (Meld (ClosedKan tiles)) = tiles

isOpen :: Meld -> Bool
isOpen (Meld (ClosedKan _)) = False
isOpen _ = True