module Main where

import Lib
import Holdem
import Card

game1 = Game
  {
    numPlayers = 4
  , flop = Just (Flop (newCard King Diamond) (newCard Queen Diamond) (newCard Jack Spade))
  , turn = Nothing
  , street = Nothing
  , players = [
        Player (Just $ newCard Ace Diamond) (Just $ newCard Ace Spade)
      , Player Nothing Nothing
      , Player Nothing Nothing
      , Player Nothing Nothing
      ]
  }
main :: IO ()
main = do
  result <- play game1 10000
  print result
