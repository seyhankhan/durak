module Game (module Game) where

import Card
import Player
import Shuffle
import System.Random (StdGen)

data Battle = Battle Card (Maybe Card) deriving (Eq)

newtype Battles = Battles [Battle] deriving (Eq)

instance Show Battle where
  show (Battle a d) = show a ++ maybe "!" (('/' :) . show) d

data GameState = GameState
  { talon :: Talon
  , players :: Players
  , battlefield :: [Battle]
  , trumpCard :: Card
  , winners :: [PlayerId]
  }
  deriving (Eq, Show)

initGameState :: StdGen -> Int -> GameState
initGameState gen n = GameState talon players [] trump
 where
  (trump : talon, gen') = shuffle deck gen
  players = fromList (Player <$> [0 .. n - 1] <*> [[]])

dealCards :: GameState -> GameState
dealCards gs@GameState{players = [p], battlefield = []} = error "Only one player left?"
dealCards gs@GameState{talon = t, players = Players ps def att, battlefield = []} =
  gs{talon = t', ps = Players (init ps') att' (last ps')}
 where
  (att' : ps', t') = dealCards' (att : ps ++ [def]) t
dealCards _ = error "Battlefield must be empty before dealing cards"

dealCards' :: [Player] -> Talon -> ([Player], Talon)
dealCards' [] talon = ([], talon)
dealCards' ps [] = (ps, [])
dealCards' (p : ps) talon = (p' : ps', talon')
 where
  (p', remainingTalon) = dealCards'' p talon
  (ps', talon') = dealCards' ps remainingTalon

dealCards'' :: Player -> Talon -> (Player, Talon)
dealCards'' p talon
  | cardsToDraw <= 0 = (p, talon)
  | otherwise =
      let (takenCards, remainingTalon) = splitAt cardsToDraw talon
       in (p{hand = hand p ++ takenCards}, remainingTalon)
 where
  cardsToDraw = 6 - length (hand p)
