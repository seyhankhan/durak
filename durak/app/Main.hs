{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import AgentHuman
import AgentLowest
import AgentMaxn
import AgentRandom
import Card
import Control.Monad
import Engine
import Game
import Shuffle
import System.Environment
import System.Random (StdGen, mkStdGen, newStdGen, randomRIO)

numGames :: Int
numGames = 500

main :: IO ()
main = do
  let initialStats = Stats 0 0 0

  -- Generate a random seed for each game
  seeds <- mapM (\_ -> newStdGen) [1 .. numGames]

  -- Run games & stats
  finalStats <-
    foldM
      ( \stats seed -> do
          let count = gameCount stats

          numPlayers <- randomRIO (2,5) -- Type any random number of players (must be ≥2)
          (if count `mod` 5 == 0 then (putStr $ "\n" ++ show count ++ " total Ls\n" ++ show stats) else return ())
          (putStr $ "\n" ++ show numPlayers ++ "-players: ")
          -- Randomise the agent players for each game
          thePlayers <- getRandomPlayers numPlayers 
          -- Initialise game state with shuffled deck and random players
          let gs = dealCards (initGameState seed thePlayers)
          finalGameState <- turn gs
          let losers = toList (players finalGameState)
              stats' = updateStats stats losers
          return stats'
      )
      initialStats
      seeds

  -- Print or use final statistics
  putStrLn $ "Final statistics after " ++ show numGames ++ " games:"
  putStrLn $ "Lowest Agent Losses: " ++ show (lowestAgentLosses finalStats)
  putStrLn $ "Max^n Agent Losses: " ++ show (maxnAgentLosses finalStats)
  putStrLn $ "Random Agent Losses: " ++ show (randomAgentLosses finalStats)

-- Define statistics data structure
data Stats = Stats
  { lowestAgentLosses :: Int,
    maxnAgentLosses :: Int,
    randomAgentLosses :: Int
  }
  deriving (Show)

gameCount :: Stats -> Int
gameCount (Stats a b c) = a + b + c

updateStats :: Stats -> [Player] -> Stats
updateStats stats losers =
  let lowestLosses = length $ filter (\p -> name p == "Lowest" && length (hand p) > 0) losers
      maxnLosses = length $ filter (\p -> name p == "max^n" && length (hand p) > 0) losers
      randomLosses = length $ filter (\p -> name p == "Random" && length (hand p) > 0) losers
   in Stats
        { lowestAgentLosses = lowestAgentLosses stats + lowestLosses,
          maxnAgentLosses = maxnAgentLosses stats + maxnLosses,
          randomAgentLosses = randomAgentLosses stats + randomLosses
        }

-- Game initialization with shuffled deck
initGameState :: StdGen -> [Player] -> GameState
initGameState _ [] = error "Need ≥2 players (not 0)"
initGameState _ [_] = error "Need ≥2 players (not 1)"
initGameState gen players = GameState talon (fromList players) [] (last talon) [] (length players)
  where
    (talon, gen') = shuffle deck gen

-- Get a random selection of players
getRandomPlayers :: Int -> IO [Player]
getRandomPlayers numPlayers = do
  let playerTypes = [genLowestAgent, genLowestAgent, genMaxnAgent, genRandomAgent]
  randomPlayerTypes <- mapM (\_ -> randomRIO (0, length playerTypes - 1)) [1 .. numPlayers]
  let uniqueIds = [0 .. numPlayers - 1]
  return $ zipWith (\f i -> f i) (map (playerTypes !!) randomPlayerTypes) uniqueIds

-- RUN MANUALLY

-- main :: IO ()
-- main = do
--   let n = 2 -- n-players
--   seed <- getArgs >>= \case
--     [] -> newStdGen
--     [seedArg] -> return (mkStdGen (read seedArg :: Int))
--   let gs = dealCards (initGameState seed n)
--   finalGameState <- turn gs
--   return ()

-- initGameState :: StdGen -> Int -> GameState
-- initGameState gen n = GameState talon players [] (last talon) [] n
--  where
--   (talon, gen') = shuffle deck gen
--   players = fromList [genLowestAgent 0, genHumanAgent 1]
