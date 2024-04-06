module Card (module Card) where

import Data.List (intercalate)

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

instance Show Card where
  show (Card r s) = show r ++ show s

newtype Cards = Cards [Card] deriving (Eq)

instance Show Cards where
  show (Cards cs) = '[' : intercalate ", " (map show cs) ++ "]"

deck :: Cards
deck = Cards $ Card <$> [Six .. Ace] <*> [Spades .. Diamonds]

type PlayerId = Int

data Player = Player {id :: PlayerId, hand :: Cards} deriving (Eq, Show)

data Battle = Battle Card (Maybe Card) Trump deriving (Eq)

instance Ord Battle where
    compare (Battle a Nothing) (Battle a Nothing) = EQ
    compare (Battle _ Nothing) (Battle _ _)       = LT
    compare (Battle _ _)       (Battle _ Nothing) = GT
    compare _                  _                  = EQ

newtype Battles = Battles [Battle] deriving (Eq)



instance Show Battle where
  show (Battle a d') =
    show a
      ++ ( case d' of
             Just d -> '/' : show d
             Nothing -> "!"
         )

instance Show Battles where
  show (Battles bs) = 

data GameState = GameState
  { attacker :: PlayerId,
    defender :: PlayerId,
    talon :: Cards,
    discard :: Cards, -- ?
    players :: [Player],
    battlefield :: [Battle],
    trumpCard :: Card
  }

someFunc :: IO ()
someFunc = putStrLn "somefFunc"
