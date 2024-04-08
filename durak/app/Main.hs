{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Shuffle
import Card
import Game
import System.Random (mkStdGen, newStdGen)
import System.Environment
import qualified Data.CircularList as CL

main :: IO ()
main = do
  seed <- getArgs >>= \case
    [] -> newStdGen
    [seedArg] -> return (mkStdGen (read seedArg :: Int))
  let gs = initGameState seed 6
  print $ players gs
  print $ CL.rotL $ players gs
  return ()
