module Player (module Player) where

import Card (Card(Card), Cards, Hand, Trump, isTrump, suit)
import Data.List (findIndex, minimumBy, sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)

type PlayerId = Int

data Player = Player {pId :: PlayerId, name :: String, hand :: Hand} deriving (Eq)

instance Show Player where
  show (Player i n h) = show i ++ ":\t" ++ show (sortBy compareCards h)
    where
      compareCards (Card r s) (Card r' s')
        | s == s' = compare r r'
        | otherwise = compare s s'

instance Ord Player where
  compare Player{pId=i} Player{pId=i'} = compare i i'

-- Players [2,3,4] 1 0
--    2  3
--   1    4
--     0

-- [Player] is a clockwise list of the rest, starting from the defender's left

data Players = Players {rest :: [Player], defender :: Player, attacker :: Player} deriving (Eq, Show)

size :: Players -> Int
size Players{rest} = 2 + length rest

toList :: Players -> [Player]
toList (Players ps d a) = a : d : ps

fromList :: [Player] -> Players
fromList [] = error "Insufficient players (Expected: ≥2)"
fromList [_] = error "Insufficient players (Expected: ≥2)"
fromList (a : d : ps) = Players ps d a

rotateOnce :: Players -> Players
rotateOnce (Players [] d a) = Players [] a d
rotateOnce (Players (d' : ps) d a) = Players (ps ++ [a]) d' d

rotateTwice :: Players -> Players
rotateTwice ps@(Players [] _ _) = ps
rotateTwice (Players [a'] d a) = Players [d] a a'
rotateTwice (Players (a' : d' : ps) d a) = Players (ps ++ [a, d]) d' a'

setAttacker :: PlayerId -> Players -> Players
setAttacker i ps@(Players _ d a)
  | pId a == i = ps
  | otherwise =
      let (before, after) = break ((i ==) . pId) (toList ps)
       in fromList (after ++ before)

--

findLowestCardPlayer :: [Player] -> Trump -> (PlayerId, Maybe Card)
findLowestCardPlayer players t = case filter (isJust . snd) smallestTrumps of
  [] -> (0, Nothing)
  cs -> minimumBy (comparing snd) cs
 where
  smallestTrumps = map (smallestTrump t) players

smallestTrump :: Trump -> Player -> (PlayerId, Maybe Card)
smallestTrump t (Player pid hand) = (pid, minimumMaybe $ filter (isTrump t) hand)

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe l = Just $ minimum l