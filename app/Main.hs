module Main where

import Cards
import PokerHands

swapCards :: [Card] -> [Card] -> IO [Card]
swapCards first_hand hand = do
  let print_hand = show $ map (\x -> if elem x hand then x else NullCard) first_hand
  putStr $ print_hand ++ " - Swap: "
  input <- getLine
  if length input > 0 && ((read input) < 1 || (read input > 5)) then
    swapCards first_hand hand
  else if length input > 0 then
    let to_be_swapped = read input
        card = first_hand !! (to_be_swapped - 1)
        second_hand = filter (\x -> x /= card) hand in
    swapCards first_hand second_hand
  else return hand

main :: IO ()
main = playLoop

playLoop :: IO ()
playLoop = do
  deck <- shuffleCards
  (p1_hand, p2_hand, deck) <- dealTwo 5 deck

  putStrLn "PLAYER 1'S TURN"
  p1_hand <- swapCards p1_hand p1_hand
  (p1_new_cards, deck) <- doDeal (5 - length p1_hand) deck

  let p1_second_hand = p1_hand ++ p1_new_cards
  putStrLn $ "Player 1's hand: " ++ (show p1_second_hand)
  let p1_result = bestHand p1_second_hand
  if p1_result == Shark then putStrLn $ (show $ p1_result) ++ getShark p1_second_hand
  else putStrLn $ show $ p1_result

  putStrLn "PLAYER 2'S TURN"

  p2_hand <- swapCards p2_hand p2_hand
  (p2_new_cards, deck) <- doDeal (5 - length p1_hand) deck

  let p2_second_hand = p2_hand ++ p2_new_cards
  putStrLn $ "Player 2's hand: " ++ (show p2_second_hand)
  let p2_result = bestHand p2_second_hand
  if p2_result == Shark then putStrLn $ (show $ p2_result) ++ getShark p2_second_hand
  else putStrLn $ show $ p2_result

  putStrLn $ decideWinner p1_second_hand p2_second_hand p1_result p2_result

  putStr "Play again? (y, n) "
  answer <- getLine
  if answer == "n" then return () else playLoop

