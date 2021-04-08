module PokerHands where

import Data.List ( groupBy, sortBy, nub, sort )
import Data.Ord ( comparing )
import Data.Maybe (fromMaybe)

import Cards

data HandResult = RoyalFlush | StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPairs | Pair | Shark deriving (Eq)

instance Show HandResult where
  show RoyalFlush = "Royal Flush"
  show StraightFlush = "Straight Flush"
  show FourOfAKind = "Four of a kind"
  show FullHouse = "Full House"
  show Flush = "Flush"
  show Straight = "Straight"
  show ThreeOfAKind = "Three of a kind"
  show TwoPairs = "Two Pairs"
  show Pair = "Single pair"
  show Shark = "Nothing! Your highest rank was: "

groupHand hand =
    let equal_rank        = (\x y -> value x == value y)
        groups            = groupBy equal_rank $ sortBy (comparing value) hand in
        reverse $ sortBy (comparing length) groups

bestHand :: [Card] -> HandResult
bestHand hand = rankMatches hand

isTwoPairs :: [Card] -> Bool
isTwoPairs hand =
  let grouped_hand = groupHand hand
      biggest_group = grouped_hand !! 0
      second_biggest_group = grouped_hand !! 1
  in
  if length biggest_group == 2 && length second_biggest_group == 2 then True
  else False

isPair :: [Card] -> Bool
isPair hand =
  let biggest_group = groupHand hand !! 0
  in
  if length biggest_group == 2 then True
  else False

isThree :: [Card] -> Bool
isThree hand =
  let biggest_group = groupHand hand !! 0
  in
  if length biggest_group == 3 then True
  else False

isFour :: [Card] -> Bool
isFour hand =
  let biggest_group = groupHand hand !! 0
  in
  if length biggest_group == 4 then True
  else False

isFullHouse :: [Card] -> Bool
isFullHouse hand =
  let grouped_hand = groupHand hand
      biggest_group = grouped_hand !! 0
      second_biggest_group = grouped_hand !! 1
  in
  if length biggest_group == 3 && length second_biggest_group == 2 then True else False

isFlush :: [Card] -> Bool
isFlush hand = case length $ nub $ map suit hand of 1 -> True
                                                    otherwise -> False
isStraight :: (Enum a, Eq a) => [a] -> Bool
isStraight [] = True
isStraight (x:[]) = True
isStraight (x:y:zs) | y == succ x = isStraight $ y:zs
isStraight _ = False

isStraightFlush :: [Card] -> Bool
isStraightFlush hand = if sortAndCheckStraight hand && isFlush hand then True else False

isRoyalFlush :: [Card] -> Bool
isRoyalFlush hand = if sortAndCheckStraight hand && isFlush hand && getShark hand == "14" then True else False

getShark :: [Card] -> String
getShark hand = show $ maximum $ map getNumericValue hand

sortAndCheckStraight :: [Card] -> Bool
sortAndCheckStraight hand = isStraight $ sort $ map getNumericValue hand

decideWinner :: [Card] -> [Card] -> HandResult -> HandResult -> String
decideWinner p1_hand p2_hand p1_result p2_result = if (getHandNumeric p1_result) == (getHandNumeric p2_result) then
                                                      if (maximum $ map getNumericValue p1_hand) == (maximum $ map getNumericValue p2_hand)
                                                      then "It's a tie!"
                                                      else if (maximum $ map getNumericValue p1_hand) > (maximum $ map getNumericValue p2_hand)
                                                      then "Player 1 wins!" else "Player 2 wins!"
                                                   else if (getHandNumeric p1_result) > (getHandNumeric p2_result)
                                                      then "Player 1 wins!" else "Player 2 wins!"


rankMatches :: [Card] -> HandResult
rankMatches hand
    | isRoyalFlush hand = RoyalFlush
    | isStraightFlush hand = StraightFlush
    | isFour hand = FourOfAKind
    | isFullHouse hand = FullHouse
    | isFlush hand = Flush
    | sortAndCheckStraight hand = Straight
    | isThree hand = ThreeOfAKind
    | isTwoPairs hand = TwoPairs
    | isPair hand = Pair
    | otherwise = Shark

getHandNumeric :: HandResult -> Int
getHandNumeric result = case result of Shark -> 0
                                       Pair -> 1
                                       TwoPairs -> 2
                                       ThreeOfAKind -> 3
                                       Straight -> 4
                                       Flush -> 5
                                       FullHouse -> 6
                                       FourOfAKind -> 7
                                       StraightFlush -> 8
                                       RoyalFlush -> 9