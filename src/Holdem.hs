{-# LANGUAGE DerivingStrategies #-}

module Holdem where

import           Card
import           Control.Monad.State


data Hole = Hole !Card !Card deriving stock (Show, Eq)

data Flop = Flop !Card !Card !Card deriving stock (Show, Eq)

newtype Turn = Turn Card deriving stock (Show, Eq)

newtype Street = Street Card deriving stock (Show, Eq)

data Community = Community !Flop !Turn !Street deriving stock (Show, Eq)

data Hand = Hand !Hole !Community deriving stock (Show, Eq)

data Player = Player
  {
    card1 :: Maybe Card
  , card2 :: Maybe Card
  }

-- | An abstraction that represents a poker game, with some unknowns.
data Game = Game
  { numPlayers :: Int           -- ^ Number of players in the game.
  , players    :: [Player]      -- ^ Other players.
  , flop       :: Maybe Flop    -- ^ The flop if it happened. Nothing otherwise.
  , turn       :: Maybe Turn    -- ^ The turn if it happened. Nothing otherwise.
  , street     :: Maybe Street  -- ^ The street if it happened. Nothing otherwise.
  }

collectDealtCards Game{..} = let playerCards = (concatMap dealtHands players)
                                 dealtHands (Player Nothing Nothing) = []
                                 dealtHands (Player (Just c1) Nothing) = [c1]
                                 dealtHands (Player Nothing (Just c2)) = [c2]
                                 dealtHands (Player (Just c1) (Just c2)) = [c1, c2]
                                 flopCards = case flop of
                                   (Just (Flop c1 c2 c3)) -> [c1, c2, c3]
                                   _ -> []
                                 turnCard = case turn of
                                   (Just (Turn c)) -> [c]
                                   _ -> []
                                 streetCard = case street of
                                   (Just (Street c)) -> [c]
                                   _ -> []
                             in playerCards ++  flopCards ++ turnCard ++ streetCard

completeHands :: Game -> StateT Deck Maybe [[Card]]
completeHands Game{..} = do
  playersCards <- mapM getPlayersCards players
  (Community (Flop c3 c4 c5) (Turn c6) (Street c7)) <- getCommunityCards flop turn street
  return [[c1, c2, c3, c4, c5, c6, c7] | (Hole c1 c2) <- playersCards]

getPlayersCards :: Player -> StateT Deck Maybe Hole
getPlayersCards player = do
  deck <- get
  (cards, deck') <- lift $ draw1 (neededCards player) deck
  put deck'
  return (hole player cards)
  where neededCards (Player Nothing Nothing)   = 2
        neededCards (Player (Just _) Nothing)  = 1
        neededCards (Player Nothing (Just _))  = 1
        neededCards (Player (Just _) (Just _)) = 0
        hole (Player (Just c1) (Just c2)) []   = Hole  c1 c2
        hole (Player (Just c1) Nothing) [c2]   = Hole c1 c2
        hole (Player Nothing (Just c2)) [c1]   = Hole c1 c2
        hole (Player Nothing Nothing) [c1, c2] = Hole c1 c2
        hole _ _                               = undefined


getCommunityCards :: Maybe Flop -> Maybe Turn -> Maybe Street -> StateT Deck Maybe Community
getCommunityCards Nothing _ _ = do
  deck <- get
  ([c1, c2, c3, c4, c5], deck') <- lift $ draw1 5 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))

getCommunityCards (Just (Flop c1 c2 c3)) Nothing _ = do
  deck <- get
  ([c4, c5], deck') <- lift $ draw1 2 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))

getCommunityCards (Just (Flop c1 c2 c3)) (Just(Turn c4)) Nothing = do
  deck <- get
  ([c5], deck') <- lift $ draw1 1 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))
getCommunityCards (Just (Flop c1 c2 c3)) (Just(Turn c4)) (Just (Street c5)) = do
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))

