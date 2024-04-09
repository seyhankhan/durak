module Card (module Card) where

import Data.Char (toUpper)

data Rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show n = case n of
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
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

--

type Cards = [Card]
type Talon = Cards
type Hand = Cards

isTrump :: Trump -> Card -> Bool
isTrump = (. suit) . (==)

didAttackerWin :: Trump -> Card -> Card -> Bool
didAttackerWin t a d = d > a || suit d == t && suit a /= t

deck :: Cards
deck = Card <$> [Six .. Ace] <*> [Spades .. Diamonds]

--