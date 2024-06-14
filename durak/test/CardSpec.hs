{-# LANGUAGE OverloadedStrings #-}

module CardSpec (spec) where

import Card
import Test.Hspec

spec :: Spec
spec = do
  it "higher ranked cards beat when of the same suit" $
    beats Clubs (Card Eight Spades) (Card Six Spades) `shouldBe` True

  it "trump card beats a higher ranked non-trump" $
    beats Diamonds (Card Queen Diamonds) (Card Ace Spades) `shouldBe` True

  it "trump card beats a lower ranked trump" $
    beats Hearts (Card Ace Hearts) (Card Six Hearts) `shouldBe` True

  it "checks if card is a trump" $
    isTrump Spades (Card Seven Spades) `shouldBe` True

  it "reads a valid 6 of Hearts" $
    (read "6H" :: Card) `shouldBe` (Card Six Hearts)

  it "reads a valid Ace of Diamonds" $
    (read "AD" :: Card) `shouldBe` (Card Ace Diamonds)
