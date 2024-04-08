module Card (module Card) where

data Rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum)

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

data Suit = Spades | Clubs | Hearts | Diamonds
  deriving (Eq, Enum)

instance Show Suit where
  show Spades = "♠"
  show Clubs = "♣"
  show Hearts = "♥"
  show Diamonds = "♦"

type Trump = Suit

data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq)

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

deck :: Cards
deck = Card <$> [Six .. Ace] <*> [Spades .. Diamonds]

--