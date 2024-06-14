{-# LANGUAGE NamedFieldPuns #-}

module Engine (module Engine) where

import Battle
import Card
import Colour
import Data.List (sortBy)

type PlayerId = Int

data Player = Player
  { pId :: PlayerId,
    name :: String,
    hand :: Hand,
    getDefenceAction :: GameState -> IO DefendAction,
    getAttackAction :: GameState -> IO AttackAction
  }

instance Eq Player where
  (Player pId1 name1 hand1 _ _) == (Player pId2 name2 hand2 _ _) =
    pId1 == pId2 && name1 == name2 && hand1 == hand2

instance Show Player where
  show Player {pId, name, hand} = show pId ++ " (" ++ name ++ "):\t" ++ show (sortBy compareCards hand) ++ "\n"
    where
      compareCards (Card r s) (Card r' s')
        | s == s' = compare r r'
        | otherwise = compare s s'

instance Ord Player where
  compare Player {pId = i} Player {pId = i'} = compare i i'

-- Players [2,3,4] 1 0
--    2  3
--   1    4
--     0

-- [Player] is a clockwise list of the rest, starting from the defender's left

data Players = Players {rest :: [Player], defender :: Player, attacker :: Player} deriving (Eq, Show)

toList :: Players -> [Player]
toList (Players ps d a) = a : d : ps

fromList :: [Player] -> Players
fromList [] = error "Insufficient players (Expected: ≥2)"
fromList [_] = error "Insufficient players (Expected: ≥2)"
fromList (a : d : ps) = Players ps d a

----------------------------

data GameState = GameState
  { talon :: Talon,
    players :: Players,
    battlefield :: [Battle],
    trumpCard :: Card,
    winners :: [PlayerId],
    n :: Int -- number of players
  }
  deriving (Eq)

instance Show GameState where
  show GameState {talon, players = Players {attacker, defender, rest}, battlefield, trumpCard, winners} =
    bold "\nTalon:\t"
      ++ show trump
      ++ " / "
      ++ show talon
      ++ bold "\nplayers:\n"
      ++ "(Attacker)\t"
      ++ showPlayer attacker
      ++ "(Defender)\t"
      ++ showPlayer defender
      ++ concatMap showPlayer rest
      ++ "Battle:\t"
      ++ show battlefield
      ++ "\n"
      ++ "Winners: "
      ++ show winners
      ++ "\n"
    where
      trump = suit trumpCard
      compareCards (Card r s) (Card r' s')
        | s == trump && s /= s' = GT
        | s' == trump && s' /= s = LT
        | r == r' = compare s s'
        | otherwise = compare r r'
      showPlayer Player {pId, name, hand} = show pId ++ " (" ++ name ++ "):\t" ++ show (sortBy compareCards hand) ++ "\n"
