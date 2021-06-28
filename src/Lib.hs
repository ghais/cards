module Lib  where
import           Control.Monad
import           Data.Binary
import           Data.Random
import           Data.Random.Source.DevRandom
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.Primitive



import qualified Holdem.Table as Holdem
import Data.Bits ((.|.), shift, shiftL, (.&.))
import Numeric
import Data.Char (intToDigit)
import Data.Foldable

data Suite = Club | Diamond | Heart | Spade deriving (Read, Show, Eq, Enum, Ord)
data Rank  = Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
           | Ace deriving (Read, Show, Eq, Enum, Ord)

data Card = Card Rank Suite deriving (Read, Show, Eq, Ord)

newtype Hand = Hand [Card] deriving (Read, Show, Eq, Ord)

newCard :: Rank -> Suite -> Int
newCard rank suite = (fromEnum rank) * 4 + (fromEnum suite)

printEncodedCard :: Rank -> Suite -> String
printEncodedCard rank suite = showIntAtBase 2 intToDigit (newCard rank suite) ""
rankBit :: V.Vector Int
rankBit = V.fromList [
    0x1,  0x1,  0x1,  0x1,
    0x2,  0x2,  0x2,  0x2,
    0x4,  0x4,  0x4,  0x4,
    0x8,  0x8,  0x8,  0x8,
    0x10,  0x10,  0x10,  0x10,
    0x20,  0x20,  0x20,  0x20,
    0x40,  0x40,  0x40,  0x40,
    0x80,  0x80,  0x80,  0x80,
    0x100,  0x100,  0x100,  0x100,
    0x200,  0x200,  0x200,  0x200,
    0x400,  0x400,  0x400,  0x400,
    0x800,  0x800,  0x800,  0x800,
    0x1000,  0x1000,  0x1000,  0x1000]

suiteBit :: V.Vector Int
suiteBit = V.fromList [
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200,
  0x1,  0x8,  0x40,  0x200]

someFunc :: IO ()
someFunc = do
  card <- replicateM 7 $ runRVar randomCard DevRandom
  print card

randomCard :: RVar Int
randomCard = do
  suite <- uniform 0 3
  rank  <- uniform 0 12
  return $ newCard (toEnum rank) (toEnum suite)

randomHand = replicateM 7 randomCard




evaluate7Cards cards =
  if (Holdem.suites V.! suitHash) > 0 then
    handleFlush cards suitHash
  else
    handleNonFlush cards
  where
    suitHash :: Int
    suitHash = sum $ map (suiteBit V.!)  cards


choose = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)


handleFlush :: (PrimMonad m) => [Int] -> Int -> m Int
handleFlush [c1, c2, c3, c4, c5, c6, c7] suitHash = do
  suitBinary <- MV.replicate 4 0
  MV.modify suitBinary (\x -> x .|. rankBit V.! c1) (c1 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c2) (c2 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c3) (c3 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c4) (c4 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c5) (c5 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c6) (c6 .&. 0x3)
  MV.modify suitBinary (\x -> x .|. rankBit V.! c7) (c7 .&. 0x3)
  idx <- MV.read suitBinary ((Holdem.suites V.! suitHash) - 1)
  return $ Holdem.flushTable V.! idx


handleNonFlush :: (PrimMonad m) => [Int] -> m Int
handleNonFlush [c1, c2, c3, c4, c5, c6, c7] = do
  quinary <- MV.replicate 13 0
  MV.modify quinary (+1) (c1 `shift` (-2))
  MV.modify quinary (+1) (c2 `shift` (-2))
  MV.modify quinary (+1) (c3 `shift` (-2))
  MV.modify quinary (+1) (c4 `shift` (-2))
  MV.modify quinary (+1) (c5 `shift` (-2))
  MV.modify quinary (+1) (c6 `shift` (-2))
  MV.modify quinary (+1) (c7 `shift` (-2))
  hash <- hashQuinary <$> V.freeze quinary
  return $ Holdem.noflush V.! hash


hashBinary :: [Bool]
           -> Int
           -> Int
           -> Int
hashBinary q n k = go q n k 0

  where
    go :: [Bool] -> Int -> Int -> Int -> Int
    go (x:xs) n' k' sum = if x then
                            go xs (n' -1) (k'-1) (sum + ((n'-1) `choose` k'))
                          else
                            go xs (n' - 1) k' sum
    go [] _ _ sum       = sum


hashQuinary :: V.Vector Int -> Int
hashQuinary q = go 7 0 0 where
  go k i sum | i >= 13 = sum
             | k <= 0  = sum
             | otherwise =
               let sum' = sum + (Holdem.dpLookup q i k)
                   k'   = k - (q V.! i)
                   i'   = i + 1
               in go k' i' sum'




holes :: Int -> RVar [[Int]]
holes n = do
  replicateM n hole where
    hole = do
      c1 <- randomCard
      c2 <- randomCard
      return [c1, c2]

data Hole = Hole Int Int
data Flop = Flop Int Int Int
newtype Turn = Turn Int
newtype Street = Street Int

data Game = Game Int Hole (Maybe Flop) (Maybe Turn) (Maybe Street)

hands :: Game -> RVar [[Int]]
hands (Game players (Hole c1 c2) Nothing _ _) = undefined
hands (Game players (Hole c1 c2) (Just (Flop c3 c4 c5)) Nothing _) = undefined
hands (Game players (Hole c1 c2) (Just (Flop c3 c4 c5)) (Just (Turn c6)) Nothing) = undefined
hands (Game players (Hole c1 c2) (Just (Flop c3 c4 c5)) (Just (Turn c6)) (Just (Street c7))) = do
  let community = [c3, c4, c5, c6, c7]
      player = c1:c2:community
  otherCards <- holes (players - 1)
  return $ player : (map (\hole -> hole ++ community)  otherCards)




hand1 = [newCard Nine Club, newCard Four Club, newCard Four Spade, newCard Nine Diamond, newCard Four Heart, newCard Queen Club, newCard Six Club]
hand2 = [newCard Nine Club, newCard Four Club, newCard Four Spade, newCard Nine Diamond, newCard Four Heart, newCard Two Club, newCard Nine Heart]

royalFlush = [newCard Ace Club, newCard King Club, newCard Queen Club, newCard Jack Club, newCard Ten Club, newCard Two Heart, newCard Six Club]
fourOfaKind = [newCard Ace Club, newCard Ace Heart, newCard Ace Diamond, newCard Ace Spade, newCard Ten Club, newCard Two Heart, newCard Six Club]
fourOfaKind2 = [newCard King Club, newCard King Heart, newCard King Diamond, newCard King Spade, newCard Ten Club, newCard Two Heart, newCard Six Club]
