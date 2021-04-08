module Cards where

import Shuffle
import Data.List

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Enum, Ord)

data Suit = Spades | Clubs | Hearts | Diamonds deriving (Eq)

data Card = Card {value :: Value, suit :: Suit} | NullCard deriving (Eq)

instance Show Value where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

instance Show Suit where
  show Spades = "S"
  show Clubs = "C"
  show Hearts = "H"
  show Diamonds = "D"

instance Show Card where
  show NullCard = "--"
  show card = show (value card) ++ "" ++ show (suit card)

allCards :: [Card]
allCards = [ Card value suit
            | suit <- [Spades, Clubs, Hearts, Diamonds]
            , value <- [Two .. Ace] ]

shuffleCards :: IO [Card]
shuffleCards = knuthShuffle allCards

deal :: Int -> [Card] -> ([Card], [Card])
deal n deck = splitAt n deck

doDeal :: Int -> [Card] -> IO ([Card], [Card])
doDeal n deck = do return (deal n deck)

dealTwo :: Int -> [Card] -> IO ([Card], [Card], [Card])
dealTwo n deck = do
  (first, tmp_rest) <- doDeal n deck
  (second, rest) <- doDeal n tmp_rest
  return (first, second, rest)

getNumericValue :: Card -> Int
getNumericValue card = case value card of Two -> 2
                                          Three -> 3
                                          Four -> 4
                                          Five -> 5
                                          Six -> 6
                                          Seven -> 7
                                          Eight -> 8
                                          Nine -> 9
                                          Ten -> 10
                                          Jack -> 11
                                          Queen -> 12
                                          King -> 13
                                          Ace -> 14
