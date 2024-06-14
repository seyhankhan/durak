{-# LANGUAGE NamedFieldPuns #-}

module Player (module Player) where

import Card
import Data.List (minimumBy, partition)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Engine

-- Players [2,3,4] 1 0
--    2  3
--   1    4
--     0

-- [Player] is a clockwise list of the rest, starting from the defender's left

rotateOnce :: Players -> Players
rotateOnce (Players [] d a) = Players [] a d
rotateOnce (Players (d' : ps) d a) = Players (ps ++ [a]) d' d

rotateTwice :: Players -> Players
rotateTwice ps@(Players [] _ _) = ps
rotateTwice (Players [a'] d a) = Players [d] a a'
rotateTwice (Players (a' : d' : ps) d a) = Players (ps ++ [a, d]) d' a'

findLowestCardPlayer :: [Player] -> Trump -> (PlayerId, Maybe Card)
findLowestCardPlayer players t = case filter (isJust . snd) smallestTrumps of
  [] -> (0, Nothing)
  cs -> minimumBy (comparing snd) cs
  where
    smallestTrumps = map (smallestTrump t) players

smallestTrump :: Trump -> Player -> (PlayerId, Maybe Card)
smallestTrump t (Player {pId, hand}) = (pId, minimumMaybe $ filter (isTrump t) hand)

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe l = Just $ minimum l

splitPlayersAndWinners :: [Player] -> ([Player], [Player])
splitPlayersAndWinners players = partition (not . null . hand) players
