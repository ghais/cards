{-# LANGUAGE FlexibleContexts #-}

module Lib  where
import           Card
import           Control.Monad
import           Data.Random
import           Holdem

import           Control.Monad.Primitive
import           Control.Monad.State
import           Data.List
import           Data.Ord
import           Data.Random.Source.DevRandom
import           Holdem.Evaluate



playerHands :: Game -> RVar [[Card]]
playerHands game = do
  deck <- Card.shuffle (gameDeck game)
  case evalStateT (completeHands game) deck of
    (Just cards) -> return cards
    _            -> return []

gameDeck game = remove (collectDealtCards game) fullDeck




minIndex :: (Ord a) => [a] -> Int
minIndex = fst . minimumBy (comparing snd) . zip [0..]

winners :: [Int] -> [Bool]
winners scores = map (== minRank) scores where
  minRank = minimum scores

averageScore :: Int -> [Bool] -> [Double]
averageScore numPlayers winnerList = map (\x -> if x then 1/fromIntegral numWinners else 0) winnerList where
  numWinners = length (filter (== True) winnerList) 


simulate :: (RandomSource m DevRandom, PrimMonad m) => Game -> m [([Card], Int)]
simulate game = do
  cards <- runRVar (playerHands game) DevRandom
  scores <- mapM evaluate cards
  return $ zip cards scores

simulateWinners :: (RandomSource m DevRandom, PrimMonad m) => Game -> m [Double]
simulateWinners game@Game{..} = do
  scores <- map snd <$> simulate game
  let gameWinners = winners scores
  return $ averageScore numPlayers gameWinners



play :: (RandomSource m DevRandom, PrimMonad m) => Game -> Int -> m [Double]
play game@Game{..} n = do
  gameHands <- replicateM n $ simulateWinners game
  --result <- map (map evaluate7Cards) myhands
  --return result
  return $ map (/fromIntegral n) (map sum (transpose gameHands))




