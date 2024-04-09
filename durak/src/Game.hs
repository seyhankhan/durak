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
dealCards gs@GameState{talon = t, players = Players ps def att, battlefield = []} =
  gs{talon = t', ps = Players (init ps') att' (last ps')}
 where
  (att' : ps', t') = dealRemainingCards (att : ps ++ [def]) t
dealCards _ = error "Battlefield must be empty before dealing cards"

dealRemainingCards :: [Player] -> Talon -> ([Player], Talon)
dealRemainingCards [] talon = ([], talon)
dealRemainingCards ps [] = (ps, [])
dealRemainingCards (p : ps) talon = (p' : ps', talon')
 where
  (p', remainingTalon) = drawCards p talon
  (ps', talon') = dealRemainingCards ps remainingTalon

drawCards :: Player -> Talon -> (Player, Talon)
drawCards p talon
  | cardsToDraw <= 0 = (p, talon)
  | otherwise =
      let (takenCards, remainingTalon) = splitAt cardsToDraw talon
       in (p{hand = hand p ++ takenCards}, remainingTalon)
 where
  cardsToDraw = 6 - length (hand p)
