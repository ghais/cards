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






randomCard :: RVar Card
randomCard = do
  suit <- uniform 0 3
  rank  <- uniform 0 12
  return $ newCard (toEnum rank) (toEnum suit)

randomHand = do
  hole <- liftM2 Hole randomCard  randomCard
  flop <- liftM3 Flop randomCard randomCard randomCard
  turn <- Turn <$> randomCard
  street <- Street  <$> randomCard
  return $ Hand hole (Community flop turn street)





playerHands :: Game -> RVar [[Card]]
playerHands game = do
  deck <- Card.shuffle (gameDeck game)
  case evalStateT (completeHands game) deck of
    (Just cards) -> return cards
    _            -> return []



bar :: (Monoid a) => Maybe a -> IO a
bar (Just a) = return a
bar Nothing  = mempty



game1 = Game
  {
    numPlayers = 4
  , flop = Nothing
  , turn = Nothing
  , street = Nothing
  , players = [
        Player (Just $ newCard Ace Spade) (Just $ newCard Ace Heart)
      , Player Nothing Nothing
      , Player (Just $ newCard Ace Diamond) (Just $ newCard Ace Club)
      , Player Nothing Nothing
      ]
  }


gameDeck game = remove (collectDealtCards game) fullDeck


minIndex :: (Ord a) => [a] -> Int
minIndex = fst . minimumBy (comparing snd) . zip [0..]

winners :: [Int] -> [Bool]
winners scores = map (== minRank) scores where
  minRank = minimum scores

simulate :: (RandomSource m DevRandom, PrimMonad m) => Game -> m [Int]
simulate game = do
  cards <- runRVar (playerHands game) DevRandom
  mapM evaluate cards

simulateWinners :: (RandomSource m DevRandom, PrimMonad m) => Game -> m [Int]
simulateWinners game = do
  scores <- simulate game
  return $ winners scores



play :: (RandomSource m DevRandom, PrimMonad m) => Game -> Int -> m [Double]
play game@Game{..} n = do
  gameHands <- replicateM n $ simulateWinners game
  --result <- map (map evaluate7Cards) myhands
  --return result
  let pecentIth i = (fromIntegral (length (filter (== i) gameHands))) / (fromIntegral n)
  return $ map pecentIth [0..numPlayers-1]



