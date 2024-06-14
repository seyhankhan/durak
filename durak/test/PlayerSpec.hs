{-# LANGUAGE OverloadedStrings #-}

module PlayerSpec (spec) where

import Battle
import Card
import Engine
import Player
import Test.Hspec

player :: PlayerId -> Player
player i = Player i "" [] (\_ -> return GiveUp) (\_ -> return EndAttack)

player' :: PlayerId -> Hand -> Player
player' i cards = Player i "" cards (\_ -> return GiveUp) (\_ -> return EndAttack)

specTrumpSuit = Spades
specNonTrumpSuit = Diamonds

lowTrumpCard = Card Six specTrumpSuit
highTrumpCard = Card Ace specTrumpSuit

emptyPlayer0 = player' 0 []
lowTrumpPlayer1 = player' 1 [lowTrumpCard, Card Eight specNonTrumpSuit]
highTrumpPlayer2 = player' 2 [Card Seven specNonTrumpSuit, highTrumpCard]
noTrumpPlayer3 = player' 3 [Card Ten specNonTrumpSuit, Card Nine specNonTrumpSuit]

specPlayers =
  [ emptyPlayer0,
    lowTrumpPlayer1,
    highTrumpPlayer2,
    noTrumpPlayer3
  ]

spec :: Spec
spec = do
  it "rotate players roles once" $
    rotateOnce (Players [player 2, player 3] (player 1) (player 0)) `shouldBe` (Players [player 3, player 0] (player 2) (player 1))

  it "rotates players roles twice" $
    rotateTwice (Players [player 2, player 3] (player 1) (player 0)) `shouldBe` (Players [player 0, player 1] (player 3) (player 2))

  it "selects the player with the smallest trump card to start game" $
    findLowestCardPlayer specPlayers specTrumpSuit `shouldBe` (1, Just lowTrumpCard)

  it "splits remaining players and winners with no cards" $
    splitPlayersAndWinners specPlayers `shouldBe` ([lowTrumpPlayer1, highTrumpPlayer2, noTrumpPlayer3], [emptyPlayer0])
