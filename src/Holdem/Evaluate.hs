{-# LANGUAGE DerivingVia #-}
module Holdem.Evaluate
  (
    HandRank(..)
  , evaluate
  , evaluate'
  , evaluateHand
  )
where

import           Card
import           Control.Monad.Primitive (PrimMonad)
import           Data.Bits (shift, (.&.), (.|.))
import           Data.Ord
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Array.Unboxed as Array
import           Holdem
import qualified Holdem.Table.DP as Holdem
import qualified Holdem.Table.Flush as Holdem
import qualified Holdem.Table.NoFlush as Holdem
import qualified Holdem.Table.Suit as Holdem
import Data.Array.Base (unsafeAt)


-- | Rank of a hand.
newtype HandRank = HandRank Int deriving newtype (Eq, Show, Read)
                                deriving newtype (Num)
                                deriving Ord via (Down Int)

-- | Evaluate a 7-card Texas Hold'em hand returning the rank of the hand.
evaluateHand :: (PrimMonad m) => Hand -> m HandRank
evaluateHand (Hand (Hole c1 c2) (Community (Flop c3 c4 c5) (Turn c6) (Street c7))) =
  evaluate c1 c2 c3 c4 c5 c6 c7

-- | Evaluate a 7-card hand and return the rank of that hand.
evaluate :: PrimMonad m =>
     Card  -- ^ c1
  -> Card  -- ^ c2
  -> Card  -- ^ c3
  -> Card  -- ^ c4
  -> Card  -- ^ c5
  -> Card  -- ^ c6
  -> Card  -- ^ c7
  -> m HandRank -- ^ The rank of the hand.
evaluate (Card c1) (Card c2) (Card c3) (Card c4) (Card c5) (Card c6) (Card c7) =
  if Holdem.suitsLookup hash > 0 then
    HandRank <$> handleFlush c1 c2 c3 c4 c5 c5 c7 hash
  else
    HandRank <$> handleNonFlush c1 c2 c3 c4 c5 c6 c7
  where hash = suitHash c1 c2 c3 c4 c5 c6 c7
    


-- | Evaluate a 7-card hand and return the rank of that hand.
evaluate' :: PrimMonad m =>
     Int  -- ^ c1
  -> Int  -- ^ c2
  -> Int  -- ^ c3
  -> Int  -- ^ c4
  -> Int  -- ^ c5
  -> Int  -- ^ c6
  -> Int  -- ^ c7
  -> m HandRank -- ^ The rank of the hand.
evaluate' c1 c2 c3 c4 c5 c6 c7 =
  if Holdem.suitsLookup hash > 0 then
    HandRank <$> handleFlush c1 c2 c3 c4 c5 c5 c7 hash
  else
    HandRank <$> handleNonFlush c1 c2 c3 c4 c5 c6 c7
  where
    hash = suitHash c1 c2 c3 c4 c5 c6 c7

suitHash :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
suitHash c1 c2 c3 c4 c5 c6 c7 =
  let b1 = suitBit `unsafeAt` c1
      b2 = suitBit `unsafeAt` c2
      b3 = suitBit `unsafeAt` c3
      b4 = suitBit `unsafeAt` c4
      b5 = suitBit `unsafeAt` c5
      b6 = suitBit `unsafeAt` c6
      b7 = suitBit `unsafeAt` c7
  in b1 + b2 + b3 + b4 + b5 + b6 + b7

hashQuinary :: V.Vector Int -> Int
hashQuinary q = go 7 0 0
  where
    go k i s
      | i >= 13 = s
      | k <= 0 = s
      | otherwise =
        let sum' = s + Holdem.dpLookup q i k
            k' = k - (q `V.unsafeIndex` i)
            i' = i + 1
         in go k' i' sum'

handleFlush :: PrimMonad m => Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m Int
handleFlush c1 c2 c3 c4 c5 c6 c7 hash = do
  suitBinary <- MV.replicate 4 0
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c1) (c1 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c2) (c2 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c3) (c3 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c4) (c4 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c5) (c5 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c6) (c6 .&. 0x3)
  MV.unsafeModify suitBinary (\x -> x .|. rankBit `unsafeAt` c7) (c7 .&. 0x3)
  idx <- MV.read suitBinary (Holdem.suitsLookup hash - 1)
  return $ Holdem.flushLookup idx

handleNonFlush :: PrimMonad m => Int -> Int -> Int -> Int -> Int -> Int -> Int -> m Int
handleNonFlush c1 c2 c3 c4 c5 c6 c7 = do
  quinary <- MV.replicate 13 0
  MV.unsafeModify quinary (+ 1) (c1 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c2 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c3 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c4 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c5 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c6 `shift` (-2))
  MV.unsafeModify quinary (+ 1) (c7 `shift` (-2))
  hash <- hashQuinary <$> V.freeze quinary
  return $ Holdem.noFlushLookup hash


rankBit :: Array.UArray Int Int
rankBit = Array.listArray (0, 51)
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

suitBit :: Array.UArray Int Int
suitBit =
  Array.listArray (0,51)
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
