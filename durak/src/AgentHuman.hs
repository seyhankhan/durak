{-# LANGUAGE NamedFieldPuns #-}

module AgentHuman (module AgentHuman) where

import Battle
import Card
import Colour (bold)
import Control.Monad (forM_)
import Data.List (sortBy)
import Engine
import Game

genHumanAgent :: PlayerId -> Player
genHumanAgent i =
  Player
    { pId = i,
      name = "Human",
      hand = [],
      getAttackAction = inputAttack i,
      getDefenceAction = inputDefence i
    }

inputNumber :: Int -> IO Int
inputNumber maxN = do
  option <- getLine
  case reads option of
    [(n, "")] | 0 <= n && n < maxN -> return n
    _ -> do
      print "Invalid number - try again?"
      inputNumber maxN

inputAttack :: PlayerId -> GameState -> IO AttackAction
inputAttack i gs = do
  let actions = sortBy (cmpAttacks (suit $ trumpCard gs)) (generateAttackActions gs)
  print gs
  putStrLn $ bold ("You are P" ++ show i ++ ". Choose an ATTACK.")

  forM_ (zip [0 ..] actions) (\(option, action) -> putStrLn $ bold (show option ++ ": ") ++ show action)

  choice <- inputNumber (length actions)
  return $ actions !! choice

inputDefence :: PlayerId -> GameState -> IO DefendAction
inputDefence i gs = do
  let actions = sortBy (cmpDefends (suit $ trumpCard gs)) (generateDefenceActions gs)
  print gs
  putStrLn $ bold ("You are P" ++ show i ++ ". Choose a DEFENCE.")

  forM_ (zip [0 ..] actions) (\(option, action) -> putStrLn $ bold (show option ++ ": ") ++ show action)

  choice <- inputNumber (length actions)
  return $ actions !! choice
