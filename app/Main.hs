module Main (main) where


import Holdem
import Card
import Holdem.Evaluate

import Data.Coerce

cards :: [(Card, Card, Card, Card, Card, Card, Card)]
cards = do
  a <- [0..45]
  b <- [(a+1)..46]
  c <- [(b+1)..47]
  d <- [(c+1)..48]
  e <- [(d+1)..49]
  f <- [(e+1)..50]
  g <- [(f+1)..51]
  return (Card a, coerce b, coerce c, coerce d, coerce e, coerce f, coerce g)

game1 :: Game
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

foo :: (Card, Card, Card, Card, Card, Card, Card) -> Int
foo (c1, c2, c3, c4, c5, c6, c7) = coerce $ evaluate c1 c2 c3 c4 c5 c6 c7
main :: IO ()
main = do
  -- print (length cards)
  --result <- play game1 10000
  --print result
  --print (length cards)
  --result <- sum <$> mapM foo cards
  -- result <- mapM foo cards
  -- result <- mapM foo cards:: IO [Int]
  --print (sum result)
   --x <- S.sum (S.map foo (S.each cards))
  let x = map foo cards
  print (sum x)
  --print (length x)




