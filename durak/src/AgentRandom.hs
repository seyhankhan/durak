{-# LANGUAGE NamedFieldPuns #-}

module AgentRandom (module AgentRandom) where

import Battle
import Engine
import Game
import System.Random (randomRIO)

genRandomAgent :: PlayerId -> Player
genRandomAgent i =
  Player
    { pId = i,
      name = "Random",
      hand = [],
      getAttackAction = getRandomAttack,
      getDefenceAction = getRandomDefend
    }

getRandomAttack :: GameState -> IO AttackAction
getRandomAttack gs = do
  let actions = generateAttackActions gs
  choice <- randomRIO (0, length actions - 1)
  return $ actions !! choice

getRandomDefend :: GameState -> IO DefendAction
getRandomDefend gs = do
  let actions = generateDefenceActions gs
  choice <- randomRIO (0, length actions - 1)
  return $ actions !! choice
