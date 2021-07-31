# cards

[![Haskell CI](https://github.com/ghais/cards/actions/workflows/haskell.yml/badge.svg)](https://github.com/ghais/cards/actions/workflows/haskell.yml)
[![Build Status](https://travis-ci.com/ghais/cards.svg?branch=main)](https://travis-ci.com/ghais/cards)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Monte Carlo simulator for Texas Hold'em that can simulate each player probability of winning.

The simulation can proceed for any number of known and unknown cards, for example we can run
a 10000 trajectory simulation of a 3 player game, where:
1. We know that the first player has Ace of Hear, and Ten of Diamond
2. The second player we only know that they hold the Ace of Diamond and one unknown card
3. For the third player we know neither of the two cards. 
4. In terms of community cards we only know the flop is King of Diamond, Queen of Diamond, and Jack of Spade

``` haskell
example :: IO ()
example = do
  let game = Game
        {
          players =
            [
              Player (Just $ newCard Ace Heart) (Just $ newCard Ten Diamond )
            , Player (Just $ newCard Ace Diamond) Nothing
            , Player Nothing Nothing
            ]
        , flop = Just (Flop (newCard King Diamond) (newCard Queen Diamond) (newCard Jack Spade))
        , turn = Nothing
        , street = Nothing
        }
  probabilities <- simulate game 10000
  print probabilities
```

Then run ```example``` in ghci

```
λ> example
[0.7717,0.1831,4.52e-2]
λ> 
```


## Hand Evaluation
Contains an implementation of an efficient poker hand evaluation based on the work of Henry Lee which you can find at [PokerHandEvaluator](https://github.com/HenryRLee/PokerHandEvaluator)

The implementation is relatively efficient and can evaluate all possible 133,784,560 possible poker hands in less than 10 seconds.
