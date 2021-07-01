module Holdem.Evaluate
  ( evaluate,
    evaluateHand,
  )
where

import Card
import Control.Monad.Primitive (PrimMonad)
import Data.Bits (shift, (.&.), (.|.))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Holdem
import qualified Holdem.Table.DP as Holdem
import qualified Holdem.Table.Flush as Holdem
import qualified Holdem.Table.NoFlush as Holdem
import qualified Holdem.Table.Suit as Holdem

evaluateHand :: (PrimMonad m) => Hand -> m Int
evaluateHand (Hand (Hole c1 c2) (Community (Flop c3 c4 c5) (Turn c6) (Street c7))) =
  evaluate [c1, c2, c3, c4, c5, c6, c7]

evaluate :: PrimMonad m => [Card] -> m Int
evaluate [Card c1, Card c2, Card c3, Card c4, Card c5, Card c6, Card c7] =
  if Holdem.suitsLookup suitHash > 0
    then handleFlush c1 c2 c3 c4 c5 c5 c7 suitHash
    else handleNonFlush c1 c2 c3 c4 c5 c6 c7
  where
    suitHash :: Int
    suitHash = sum $ map (suitBit V.!) cards
    cards = [c1, c2, c3, c4, c5, c6, c7]

hashQuinary :: V.Vector Int -> Int
hashQuinary q = go 7 0 0
  where
    go k i s
      | i >= 13 = s
      | k <= 0 = s
      | otherwise =
        let sum' = s + Holdem.dpLookup q i k
            k' = k - (q V.! i)
            i' = i + 1
         in go k' i' sum'

handleFlush :: PrimMonad m => Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m Int
handleFlush c1 c2 c3 c4 c5 c6 c7 suitHash = do
  suitBinary <- MV.replicate 4 0
  MV.modify suitBinary (\x -> x .|. rankBit V.! c1) (c1 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c2) (c2 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c3) (c3 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c4) (c4 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c5) (c5 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c6) (c6 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c7) (c7 .&. 0x3)
  idx <- MV.read suitBinary (Holdem.suitsLookup suitHash - 1)
  return $ Holdem.flushLookup idx

handleNonFlush :: PrimMonad m => Int -> Int -> Int -> Int -> Int -> Int -> Int -> m Int
handleNonFlush c1 c2 c3 c4 c5 c6 c7 = do
  quinary <- MV.replicate 13 0
  MV.modify quinary (+ 1) (c1 `shift` (-2))
  MV.modify quinary (+ 1) (c2 `shift` (-2))
  MV.modify quinary (+ 1) (c3 `shift` (-2))
  MV.modify quinary (+ 1) (c4 `shift` (-2))
  MV.modify quinary (+ 1) (c5 `shift` (-2))
  MV.modify quinary (+ 1) (c6 `shift` (-2))
  MV.modify quinary (+ 1) (c7 `shift` (-2))
  hash <- hashQuinary <$> V.freeze quinary
  return $ Holdem.noFlushLookup hash

choose :: Int -> Int -> Int
choose = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _ _ 0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n -1) (k -1)

hashBinary ::
  [Bool] ->
  Int ->
  Int ->
  Int
hashBinary q n k = go q n k 0
  where
    go :: [Bool] -> Int -> Int -> Int -> Int
    go (x : xs) n' k' s =
      if x
        then go xs (n' -1) (k' -1) (s + ((n' -1) `choose` k'))
        else go xs (n' - 1) k' s
    go [] _ _ s = s

rankBit :: V.Vector Int
rankBit =
  V.fromList
    [ 0x1,
      0x1,
      0x1,
      0x1,
      0x2,
      0x2,
      0x2,
      0x2,
      0x4,
      0x4,
      0x4,
      0x4,
      0x8,
      0x8,
      0x8,
      0x8,
      0x10,
      0x10,
      0x10,
      0x10,
      0x20,
      0x20,
      0x20,
      0x20,
      0x40,
      0x40,
      0x40,
      0x40,
      0x80,
      0x80,
      0x80,
      0x80,
      0x100,
      0x100,
      0x100,
      0x100,
      0x200,
      0x200,
      0x200,
      0x200,
      0x400,
      0x400,
      0x400,
      0x400,
      0x800,
      0x800,
      0x800,
      0x800,
      0x1000,
      0x1000,
      0x1000,
      0x1000
    ]

suitBit :: V.Vector Int
suitBit =
  V.fromList
    [ 0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200,
      0x1,
      0x8,
      0x40,
      0x200
    ]
