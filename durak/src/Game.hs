{-# LANGUAGE NamedFieldPuns #-}

module Game (module Game) where

import Battle
import Card
import Colour
import Data.List ((\\))
import Engine
import Player
import System.IO

dealCards :: GameState -> GameState
dealCards gs@GameState {talon = t, players = Players ps def att, battlefield = []} =
  gs {talon = t', players = Players (init ps') (last ps') att'}
  where
    (att' : ps', t') = dealRemainingCards (att : ps ++ [def]) t
dealCards gs = gs

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
       in (p {hand = hand p ++ takenCards}, remainingTalon)
  where
    cardsToDraw = maxHandSize - length (hand p)

gameOver :: GameState -> Bool
gameOver GameState {players = ps} = gameOver' (toList ps)

gameOver' :: [Player] -> Bool
gameOver' [] = True
gameOver' [_] = True
gameOver' players = length (filter (\p -> length (hand p) > 0) players) <= 1

-- 1 TURN (github.com/mildbyte/durak)
attack :: GameState -> IO (GameState, Bool)
attack gs@(GameState {players = Players {attacker = Player {getAttackAction}}}) = do
  playerChoice <- getAttackAction gs
  let newState = applyAttackAction gs playerChoice

  case playerChoice of
    EndAttack -> return (newState, True)
    _ -> defend newState

defend :: GameState -> IO (GameState, Bool)
defend gs@(GameState {players = Players {defender = Player {getDefenceAction}}}) = do
  playerChoice <- getDefenceAction gs
  let newState = applyDefenceAction gs playerChoice

  case playerChoice of
    GiveUp -> return (newState, False)
    _ -> attack newState

filterOutWinners :: GameState -> GameState
filterOutWinners gs@GameState {players = ps, winners = ws, battlefield = []} =
  let (stillPlayers, newWinners) = splitPlayersAndWinners (toList ps)
   in gs {players = fromList stillPlayers, winners = ws ++ map pId newWinners}
filterOutWinners gs = gs

-- return the final gamestate
turn :: GameState -> IO GameState
turn gs = do
  -- distribute the cards from talon
  putStr
    ( ( case (name (attacker (players gs))) of
          "max^n" -> blue
          "Random" -> red
          _ -> green
      )
        ++ "."
        ++ reset
    )
  hFlush stdout
  let gs' = dealCards gs
  if gameOver gs'
    then return gs'
    else do
      -- players still left with no cards are moved to Winners
      -- start the attack
      (gs'', _) <- attack (filterOutWinners gs')
      -- Decide who goes next based on if all cards were defended

      turn gs''

--------------------------------------------------------------------------------
-- React to game state with attack or defence (monads in case the player is human)

-- todo: save discard pile?
-- check battlefield is valid for action?

applyAttackAction :: GameState -> AttackAction -> GameState
applyAttackAction gs EndAttack =
  gs {battlefield = [], players = rotateOnce (players gs)}
applyAttackAction gs@(GameState {battlefield = bf, players = ps@Players {attacker = a}}) (Attack card) =
  gs
    { battlefield = (Battle card Nothing) : bf,
      players = ps {attacker = a {hand = hand a \\ [card]}}
    }

dropDefendCard :: Players -> Card -> Players
dropDefendCard ps@(Players {defender = d@Player {hand}}) card = ps {defender = d {hand = hand \\ [card]}}

-- Applies a defense action to the game state
-- The boolean parameter is whether player 1 performed the action.
applyDefenceAction :: GameState -> DefendAction -> GameState
applyDefenceAction (GameState {battlefield = []}) _ =
  error "Nothing to defend."
applyDefenceAction (GameState {battlefield = (Battle _ (Just _) : _)}) _ =
  error "The attack was already defended."
applyDefenceAction gs@(GameState {battlefield = (Battle a Nothing : bf), players = ps@Players {defender = d}}) (Defend card) =
  gs
    { battlefield = (Battle a (Just card)) : bf,
      players = ps {defender = d {hand = hand d \\ [card]}}
    }
applyDefenceAction gs@(GameState {battlefield = bf, players = ps@Players {defender = d}}) GiveUp =
  gs
    { battlefield = [],
      players = rotateTwice (ps {defender = d {hand = hand d ++ getCards bf}})
    }

-- Generates all possible defense actions
generateDefenceActions :: GameState -> [DefendAction]
generateDefenceActions GameState {battlefield = []} =
  error "Nothing to defend"
generateDefenceActions GameState {battlefield = (Battle _ (Just _) : _)} =
  error "All cards already defended"
generateDefenceActions (GameState {trumpCard, players, battlefield = (Battle a Nothing : _)}) =
  GiveUp : map Defend (filter (\c -> beats trumpSuit c a) $ hand (defender players))
  where
    trumpSuit = suit trumpCard

getRankMatchingCards :: Battles -> Hand -> Cards
getRankMatchingCards bs attackersHand = filter (\card -> rank card `elem` validRanks) attackersHand
  where
    validRanks = getRanks bs

-- Generates all possible attack actions:
generateAttackActions :: GameState -> [AttackAction]
generateAttackActions (GameState {players = Players {attacker, defender}, battlefield = bf})
  | null (hand attacker) = [EndAttack]
  | null (hand defender) = [EndAttack]
  | null bf = map Attack $ hand attacker
  | otherwise = EndAttack : (map Attack $ getRankMatchingCards bf (hand attacker))
