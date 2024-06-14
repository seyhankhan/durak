{-# LANGUAGE NamedFieldPuns #-}

module AgentLowest (module AgentLowest) where

import Battle
import Card
import Data.List (minimumBy)
import Engine
import Game
import System.Random (randomRIO)

genLowestAgent :: PlayerId -> Player
genLowestAgent i =
  Player
    { pId = i,
      name = "Lowest",
      hand = [],
      getAttackAction = getLowestAttack,
      getDefenceAction = getLowestDefend
    }

getLowestAttack :: GameState -> IO AttackAction
getLowestAttack gs = do
  let trumpSuit = suit $ trumpCard gs
  return $ minimumBy (cmpAttacks trumpSuit) (generateAttackActions gs)

getLowestDefend :: GameState -> IO DefendAction
getLowestDefend gs = do
  let trumpSuit = suit $ trumpCard gs
  return $ minimumBy (cmpDefends trumpSuit) (generateDefenceActions gs)
