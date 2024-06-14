{-# LANGUAGE NamedFieldPuns #-}

module AgentMaxn (module AgentMaxn) where

import Battle
import Card
import Data.List (find, maximumBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Engine
import Game

maxDepth = 12

genMaxnAgent :: PlayerId -> Player
genMaxnAgent i =
  Player
    { pId = i,
      name = "max^n",
      hand = [],
      getAttackAction = getMaxNAttackAction,
      getDefenceAction = getMaxNDefenceAction
    }

-- The ith evaluated scores of this state for the ith player
type MaxNResult = (Action, [Int])

getMaxNAttackAction :: GameState -> IO AttackAction
getMaxNAttackAction gs = do
  result <- maxnAttack 0 gs
  case result of
    (D _, _) -> error "Incompatible move"
    (A act, vs) -> do
      return act

getMaxNDefenceAction :: GameState -> IO DefendAction
getMaxNDefenceAction gs = do
  result <- maxnDefend 0 gs
  case result of
    (A _, _) -> error "Incompatible move"
    (D act, vs) -> do
      return act

-- Max^n function for the attacker
maxnAttack :: Int -> GameState -> IO MaxNResult
maxnAttack depth gs = do
  let pid = pId (attacker (players gs))
      gs' = dealCards gs
  if depth == maxDepth || gameOver gs'
    then return (A EndAttack, evaluateState gs')
    else do
      let possibleActions = generateAttackActions (filterOutWinners gs')
      results <-
        mapM
          ( \act -> do
              let newState = applyAttackAction gs' act
              if act == EndAttack
                then do
                  (_, vs) <- maxnAttack (depth + 1) newState
                  return (A act, vs)
                else do
                  (_, vs) <- maxnDefend (depth + 1) newState
                  return (A act, vs)
          )
          possibleActions
      let bestResult = maximumBy (comparing (\t -> (snd t) !! pid)) results
      return bestResult

-- Max^n function for the defender
maxnDefend :: Int -> GameState -> IO MaxNResult
maxnDefend depth gs = do
  let pid = pId (defender (players gs))

  if depth == maxDepth || gameOver gs
    then return (D GiveUp, evaluateState gs)
    else do
      let possibleActions = generateDefenceActions gs
      results <-
        mapM
          ( \act -> do
              let newState = applyDefenceAction gs act
              (_, vs) <- maxnAttack (depth + 1) newState
              return (D act, vs)
          )
          possibleActions
      let bestResult = maximumBy (comparing (\t -> (snd t) !! pid)) results
      return bestResult

numberOfCards :: Trump -> PlayerId -> [Player] -> Int
numberOfCards t pid players =
  case find (\player -> pId player == pid) players of
    Just player -> 2 * length (hand player) - (length $ filter (isTrump t) (hand player))
    Nothing -> 0

evaluateState :: GameState -> [Int]
evaluateState GameState {players, n, trumpCard} =
  [100 - numberOfCards t i (toList players) | i <- [0 .. n - 1]]
  where
    t = suit trumpCard

-- Commented out for speed testing, but can be uncommented

-- -- Evaluate the player's hand on specified criteria
-- evaluateState :: GameState -> [Int]
-- evaluateState gs@GameState{players, n, winners, trumpCard} =
--    let trump = suit trumpCard
--        evaluationMap = Map.fromList [ (pId player, evaluatePlayer gs trump player) | player <- (toList players) ]
--     in [ Map.findWithDefault 0 i evaluationMap | i <- [0..n-1] ]
-- evaluatePlayer :: GameState -> Trump -> Player -> Int
-- evaluatePlayer gs@GameState{players=Players{attacker, defender}} trump player@Player{pId=i, hand=playerHand} =
--     let cardCount = length playerHand
--         trumpBonus = length (filter (isTrump trump) playerHand)
--         -- defenderPenalty = if (i==pId defender) then
--         --                     (if all (\a -> any (beats trump a) playerHand) (hand attacker)
--         --                      then length (hand attacker)
--         --                      else 0)
--         --                   else 0
--         -- attackerBonus = if (i == pId attacker) then
--         --                     (if all (\d -> any (\a -> beats trump a d) nonTrumpCards) (hand defender)
--         --                      then -cardCount
--         --                      else 0)
--         --                   else 0
--     in 100-cardCount -- + trumpBonus -- + defenderPenalty + attackerBonus
