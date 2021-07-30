module Lib  where
import           Card (Card, Deck, remove, shuffle, stdDeck)
import           Control.Monad (replicateM)
import           Data.Random (RVar, RandomSource, runRVar)
import           Holdem (Game, completeHands, dealtCards)

import           Control.Monad.State (evalStateT)
import           Data.List (transpose)

import           Data.Random.Source.DevRandom (DevRandom (DevRandom))
import           Holdem.Evaluate (HandRank, evaluate)



playerHands :: Game -> RVar [[Card]]
playerHands game = do
  deck <- Card.shuffle (gameDeck game)
  case evalStateT (completeHands game) deck of
    (Just cards) -> return cards
    _            -> return []

gameDeck :: Game -> Deck
gameDeck game = remove (dealtCards game) stdDeck



winners :: [HandRank] -> [Bool]
winners scores = map (== minRank) scores where
  minRank = maximum scores

averageScore :: [Bool] -> [Double]
averageScore winnerList = map (\x -> if x then 1/fromIntegral numWinners else 0) winnerList where
  numWinners = length (filter (== True) winnerList)


simulate :: (RandomSource m DevRandom) => Game -> m [([Card], HandRank)]
simulate game = do
  cards <- runRVar (playerHands game) DevRandom
  let scores = map evaluate' cards
  return $ zip cards scores
  where evaluate' [c1, c2, c3, c4, c5, c6, c7] = evaluate c1 c2 c3 c4 c5 c6 c7
        evaluate' _                            = undefined

simulateWinners :: (RandomSource m DevRandom) => Game -> m [Double]
simulateWinners game = do
  scores <- map snd <$> simulate game
  let gameWinners = winners scores
  return $ averageScore gameWinners



play :: (RandomSource m DevRandom) => Game -> Int -> m [Double]
play game n = do
  gameHands <- replicateM n $ simulateWinners game
  --result <- map (map evaluate7Cards) myhands
  --return result
  return $ map ((/fromIntegral n) . sum) (transpose gameHands)




