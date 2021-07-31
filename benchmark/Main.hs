module Main (main) where

import Gauge.Main
import Poker.Holdem.Evaluate
import Poker.Deck
cards = do
  a <- [0..45]
  b <- [a+1..46]
  c <- [b+1..47]
  d <- [c+1..48]
  e <- [d+1..49]
  f <- [e+1..50]
  g <- [f+1..51]
  return (Card a, Card b, Card c, Card d, Card e, Card f, Card g)

foo (c1, c2, c3, c4, c5, c6, c7) = evaluate c1 c2 c3 c4 c5 c6 c7

main :: IO ()
main = undefined  
