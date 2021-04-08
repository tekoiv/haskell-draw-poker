module Tests where

import Test.HUnit

import Cards
import PokerHands
import Shuffle
import Data.List (nub, sort)

-- Tests for poker hands --

four_of_a_kind = [Card Jack Hearts, Card Jack Diamonds, Card Jack Spades, Card Jack Clubs, Card Ace Hearts]
three_of_a_kind = [Card Jack Hearts, Card Jack Diamonds, Card Jack Spades, Card King Clubs, Card Ten Hearts]
single_pair = [Card Jack Hearts, Card Jack Diamonds, Card Queen Spades, Card King Spades, Card Ace Spades]
two_pairs = [Card Jack Hearts, Card Jack Diamonds, Card Queen Spades, Card Queen Diamonds, Card Ace Spades]
ten_shark = [Card Nine Hearts, Card Eight Diamonds, Card Seven Spades, Card Six Clubs, Card Ten Diamonds]
ace_shark = [Card Ten Hearts, Card Six Diamonds, Card Seven Clubs, Card Eight Clubs, Card Ace Spades]
full_house = [Card Ten Hearts, Card Ten Diamonds, Card Ten Clubs, Card Seven Clubs, Card Seven Diamonds]
flush = [Card Ten Hearts, Card Nine Hearts, Card Two Hearts, Card Ace Hearts, Card King Hearts]
straight = [Card Two Hearts, Card Three Diamonds, Card Four Clubs, Card Five Clubs, Card Six Clubs]
straight_flush = [Card Two Hearts, Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts]
royal_flush = [Card Ace Spades, Card King Spades, Card Queen Spades, Card Jack Spades, Card Ten Spades]
card_with_value_2 = Card Two Hearts
card_with_value_14 = Card Ace Hearts
null_card = NullCard

best_hand_test_1 = TestCase (assertEqual "Should return RoyalFlush" (RoyalFlush) (bestHand royal_flush))
best_hand_test_2 = TestCase (assertEqual "Should return StraightFlush" (StraightFlush) (bestHand straight_flush))
best_hand_test_3 = TestCase (assertEqual "Should return Shark" (Shark) (bestHand ace_shark))
best_hand_test_4 = TestCase (assertEqual "Should return ThreeOfAKind" (ThreeOfAKind) (bestHand three_of_a_kind))
best_hand_test_5 = TestCase (assertEqual "Should return FourOfAKind" (FourOfAKind) (bestHand four_of_a_kind))
best_hand_test_6 = TestCase (assertEqual "Should return Straight" (Straight) (bestHand straight))
best_hand_test_7 = TestCase (assertEqual "Should return Pair" (Pair) (bestHand single_pair))
best_hand_test_8 = TestCase (assertEqual "Should return TwoPairs" (TwoPairs) (bestHand two_pairs))
best_hand_test_9 = TestCase (assertEqual "Should return FullHouse" (FullHouse) (bestHand full_house))
best_hand_test_10 = TestCase (assertEqual "Should return Flush" (Flush) (bestHand flush))

royal_flush_test_1 = TestCase (assertEqual "Should count as royal flush" (True) (isRoyalFlush royal_flush))
royal_flush_test_2 = TestCase (assertEqual "Should not count as royal flush" (False) (isRoyalFlush straight_flush))

straight_flush_test_1 = TestCase (assertEqual "Should count as straight flush" (True) (isStraightFlush straight_flush))
straight_flush_test_2 = TestCase (assertEqual "Should not count as straight flush" (False) (isStraightFlush flush))

straight_test_1 = TestCase (assertEqual "Should count as straight" (True) (isStraight $ map getNumericValue straight))
straight_test_2 = TestCase (assertEqual "Should not count as straight" (False) (isStraight $ map getNumericValue flush))

flush_test_1 = TestCase (assertEqual "Should count as flush" (True) (isFlush flush))
flush_test_2 = TestCase (assertEqual "Should not count as flush" (False) (isFlush ace_shark))

full_house_test_1 = TestCase (assertEqual "Should count as full house" (True) (isFullHouse full_house))
full_house_test_2 = TestCase (assertEqual "SHould not count as full house" (False) (isFullHouse ace_shark))

highest_value_test_1 = TestCase (assertEqual "Should return 10" ("10") (getShark ten_shark))
highest_value_test_2 = TestCase (assertEqual "Should return 14" ("14") (getShark ace_shark))

two_pairs_test_1 = TestCase (assertEqual "Should count as two pairs" (True) (isTwoPairs two_pairs))
two_pairs_test_2 = TestCase (assertEqual "Should count as single pair" (False) (isTwoPairs single_pair))

single_pair_test_1 = TestCase (assertEqual "Should count as single pair" (True) (isPair single_pair))
single_pair_test_2 = TestCase (assertEqual "Should not count as single pair" (False) (isPair three_of_a_kind))

three_test_1 = TestCase (assertEqual "Should count as three of a kind" (True) (isThree three_of_a_kind))
three_test_2 = TestCase (assertEqual "Should not count as three of a kind" (False) (isThree four_of_a_kind))

four_test_1 = TestCase (assertEqual "Should count as four of a kind" (True) (isFour four_of_a_kind))
four_test_2 = TestCase (assertEqual "Should not count as four of a kind" (False) (isFour three_of_a_kind))

numeric_value_test_1 = TestCase (assertEqual "Should return '2'" (2) (getNumericValue card_with_value_2))
numeric_value_test_2 = TestCase (assertEqual "Should return '14'" (14) (getNumericValue card_with_value_14))

-- Tests for deciding the winner --

p1_hand = four_of_a_kind
p2_hand = royal_flush

decide_test_1 = TestCase (assertEqual "Player 2 should win" ("Player 2 wins!") (decideWinner p1_hand p2_hand FourOfAKind RoyalFlush))

p2_hand_single = single_pair
decide_test_2 = TestCase (assertEqual "Player 1 should win" ("Player 1 wins!") (decideWinner p1_hand p2_hand_single FourOfAKind Pair))

p1_hand_ace_shark = ace_shark
decide_test_3 = TestCase (assertEqual "Player 2 should win" ("Player 2 wins!") (decideWinner p1_hand_ace_shark p2_hand Shark RoyalFlush))

p2_hand_ten_shark = ten_shark
decide_test_4 = TestCase (assertEqual "Player 1 should win" ("Player 1 wins!") (decideWinner p1_hand_ace_shark p2_hand_ten_shark Shark Shark))

-- Cards.hs tests --

show_value_test_1 = TestCase (assertEqual "Should show value 2" ("2") (show $ value card_with_value_2))
show_suit_test_1 = TestCase (assertEqual "Should show suit Hearts" ("H") (show $ suit card_with_value_2))
show_card_test_1 = TestCase (assertEqual "Should show 2 of Hearts" ("2H") (show card_with_value_2))
show_card_test_2 = TestCase (assertEqual "Should show NullCard" ("--") (show null_card))
all_cards_test_1 = TestCase (assertEqual "Should have 52 cards" (52) (length $ allCards))
all_cards_test_2 = TestCase (assertEqual "All cards unique" (52) (length $ nub $ allCards))
deal_test_1 = TestCase (assertEqual "Should return tuple with lengths n and 52-n" ((5, 47)) ((length $ fst $ deal 5 allCards, length $ snd $ deal 5 allCards)))

-- Miscellaneous --

result_1 = RoyalFlush
result_2 = Shark
result_3 = TwoPairs

hand_as_int_test_1 = TestCase (assertEqual "Should get correct number" ((9, 0, 2)) ((getHandNumeric result_1, getHandNumeric result_2, getHandNumeric result_3)))

tests = TestList [TestLabel "Four test 1" four_test_1,
                  TestLabel "Four test 2" four_test_2,
                  TestLabel "Three test 1" three_test_1,
                  TestLabel "Three test 2" three_test_2,
                  TestLabel "Single pair test 1" single_pair_test_1,
                  TestLabel "Single pair test 2" single_pair_test_2,
                  TestLabel "Two pairs test 1" two_pairs_test_1,
                  TestLabel "Two pairs test 2" two_pairs_test_2,
                  TestLabel "Numeric value test 1" numeric_value_test_1,
                  TestLabel "Numeric value test 2" numeric_value_test_2,
                  TestLabel "Shark test 1" highest_value_test_1,
                  TestLabel "Shark test 2" highest_value_test_2,
                  TestLabel "Full house test 1" full_house_test_1,
                  TestLabel "FUll house test 2" full_house_test_2,
                  TestLabel "Flush test 1" flush_test_1,
                  TestLabel "Flush test 2" flush_test_2,
                  TestLabel "Straight test 1" straight_test_1,
                  TestLabel "Straight test 2" straight_test_2,
                  TestLabel "Straight flush test 1" straight_flush_test_1,
                  TestLabel "Straight flush test 2" straight_flush_test_2,
                  TestLabel "Royal flush test 1" royal_flush_test_1,
                  TestLabel "Royal flush test 2" royal_flush_test_2,
                  TestLabel "Decide winner test 1" decide_test_1,
                  TestLabel "Decide winner test 2" decide_test_2,
                  TestLabel "Decide winner test 3" decide_test_3,
                  TestLabel "Decide winner test 4" decide_test_4,
                  TestLabel "Show value test 1" show_value_test_1,
                  TestLabel "Show suit test 1" show_suit_test_1,
                  TestLabel "Show card test 1" show_card_test_1,
                  TestLabel "Show card test 2" show_card_test_2,
                  TestLabel "All cards test 1" all_cards_test_1,
                  TestLabel "All cards test 2" all_cards_test_2,
                  TestLabel "Deal test 1" deal_test_1,
                  TestLabel "Hand as int test 1" hand_as_int_test_1,
                  TestLabel "Best hand test 1" best_hand_test_1,
                  TestLabel "Best hand test 2" best_hand_test_2,
                  TestLabel "Best hand test 3" best_hand_test_3,
                  TestLabel "Best hand test 4" best_hand_test_4,
                  TestLabel "Best hand test 5" best_hand_test_5,
                  TestLabel "Best hand test 6" best_hand_test_6,
                  TestLabel "Best hand test 7" best_hand_test_7,
                  TestLabel "Best hand test 8" best_hand_test_8,
                  TestLabel "Best hand test 9" best_hand_test_9,
                  TestLabel "Best hand test 10" best_hand_test_10]