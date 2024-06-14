module Card (module Card) where

import Data.Char (toUpper)

maxHandSize :: Int
maxHandSize = 6

data Rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show n = case n of
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "T"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance Read Rank where
  readsPrec _ s = [(r, "") | r <- [minBound .. maxBound], show r == map toUpper s]

data Suit = Spades | Clubs | Hearts | Diamonds
  deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
  show Spades = "♠"
  show Clubs = "♣"
  show Hearts = "♥"
  show Diamonds = "♦"

deck :: Cards
deck = Card <$> [Nine .. Ace] <*> [Spades .. Diamonds]

instance Read Suit where
  readsPrec _ input = case map toUpper input of
    "S" -> [(Spades, "")]
    "C" -> [(Clubs, "")]
    "H" -> [(Hearts, "")]
    "D" -> [(Diamonds, "")]
    _ -> [(s, "") | s <- [minBound .. maxBound], show s == input]

type Trump = Suit

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq)

instance Read Card where
  readsPrec _ input = [(Card (read rankStr :: Rank) (read suitStr :: Suit), "")]
    where
      (rankStr, suitStr) = splitAt (length input - 1) input

instance Ord Card where
  compare (Card r s) (Card r' s')
    | s == s' = compare r r'
    | otherwise = EQ

instance Show Card where
  show (Card r s) = show r ++ show s

type Cards = [Card]

type Talon = [Card]

type Hand = Cards

isTrump :: Trump -> Card -> Bool
isTrump = (. suit) . (==)

trumpCount :: Trump -> Hand -> Int
trumpCount t cs = length $ filter (isTrump t) cs

cardOrder :: Suit -> Card -> Card -> Ordering
cardOrder trump (Card r s) (Card r' s')
  | s == trump && s /= s' = GT
  | s' == trump && s' /= s = LT
  | r == r' = compare s s'
  | otherwise = compare r r'

-- Strict Partial Order
beats :: Suit -> Card -> Card -> Bool
beats trump (Card r1 s1) (Card r2 s2) =
  s1 == s2 && r1 > r2 || s1 == trump && s2 /= trump
