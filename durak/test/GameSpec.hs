{-# LANGUAGE OverloadedStrings #-}

module GameSpec (spec) where

import Battle
import Card
import Engine
import Game
import Test.Hspec

player :: PlayerId -> Player
player i = Player i "" [] (\_ -> return GiveUp) (\_ -> return EndAttack)

player' :: PlayerId -> Hand -> Player
player' i cards = Player i "" cards (\_ -> return GiveUp) (\_ -> return EndAttack)

genPlayers :: PlayerId -> Players
genPlayers num = fromList [player i | i <- [0 .. num - 1]]

spec :: Spec
spec = do
  describe "dealCards" $ do
    it "deals cards to players in order of priority" $ do
      let initialTalon = [Card Ace Spades, Card King Spades, Card Queen Spades]
          initialState = GameState initialTalon (genPlayers 2) [] (Card Ace Spades) [] 2
          finalState = dealCards initialState
      length (talon finalState) `shouldBe` 0
      length (hand (attacker (players finalState))) `shouldBe` 3
      length (hand (defender (players finalState))) `shouldBe` 0

  describe "gameOver" $ do
    it "returns True when only one player has cards left" $ do
      let state = GameState [] (fromList [player' 1 [], player' 2 [Card Six Hearts]]) [] (Card Ace Spades) [] 2
      gameOver state `shouldBe` True

    it "returns False when more than one player has cards left" $ do
      let state = GameState [] (fromList [player' 1 [Card Ace Spades], player' 2 [Card King Spades]]) [] (Card Ace Spades) [] 2
      gameOver state `shouldBe` False

  describe "filterOutWinners" $ do
    it "filters out players with no cards and adds them to winners" $ do
      let state = GameState [] (fromList [player' 1 [], player' 2 [Card King Spades], player' 3 [Card Ten Spades]]) [] (Card Ace Spades) [] 3
          finalState = filterOutWinners state
      length (winners finalState) `shouldBe` 1
      length (toList (players finalState)) `shouldBe` 2

  describe "applyAttackAction" $ do
    it "applies an attack action and updates the game state correctly" $ do
      let state =
            GameState
              { talon = [],
                players =
                  Players
                    { rest = [],
                      defender = Player 2 "" [Card King Spades] (const $ return GiveUp) (const $ return EndAttack),
                      attacker = Player 1 "" [Card Ace Spades] (const $ return GiveUp) (const $ return $ Attack (Card Ace Spades))
                    },
                battlefield = [],
                trumpCard = Card Ace Spades,
                winners = [],
                n = 2
              }
          finalState = applyAttackAction state (Attack (Card Ace Spades))
      length (hand (attacker (players finalState))) `shouldBe` 0
      length (battlefield finalState) `shouldBe` 1

  describe "applyDefenceAction" $ do
    it "applies a defense action and updates the game state correctly" $ do
      let state =
            GameState
              { talon = [],
                players =
                  Players
                    { rest = [],
                      defender = Player 2 "" [Card King Spades] (const $ return $ Defend (Card King Spades)) (const $ return EndAttack),
                      attacker = Player 1 "" [Card Ace Spades] (const $ return GiveUp) (const $ return $ Attack (Card Ace Spades))
                    },
                battlefield = [Battle (Card Ace Spades) Nothing],
                trumpCard = Card Ace Spades,
                winners = [],
                n = 2
              }
          finalState = applyDefenceAction state (Defend (Card King Spades))
      length (hand (defender (players finalState))) `shouldBe` 0
      length (battlefield finalState) `shouldBe` 1

  describe "generateDefenceActions" $ do
    it "generates possible defense actions correctly" $ do
      let state =
            GameState
              { talon = [],
                players =
                  Players
                    { rest = [],
                      defender = Player 2 "" [Card King Spades, Card Ten Spades] (const $ return $ Defend (Card King Spades)) (const $ return EndAttack),
                      attacker = Player 1 "" [] (const $ return GiveUp) (const $ return $ Attack (Card Jack Spades))
                    },
                battlefield = [Battle (Card Jack Spades) Nothing],
                trumpCard = Card Ace Spades,
                winners = [],
                n = 2
              }
          actions = generateDefenceActions state
      length actions `shouldBe` 2

  describe "generateAttackActions" $ do
    it "generates possible attack actions correctly" $ do
      let state =
            GameState
              { talon = [],
                players =
                  Players
                    { rest = [],
                      defender = player' 2 [Card Nine Spades],
                      attacker = player' 1 [Card Ace Spades, Card Jack Clubs]
                    },
                battlefield = [Battle (Card Ten Spades) (Just (Card Jack Spades))],
                trumpCard = Card Ace Spades,
                winners = [],
                n = 2
              }
          actions = generateAttackActions state
      length actions `shouldBe` 2
