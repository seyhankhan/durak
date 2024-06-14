{-# LANGUAGE NamedFieldPuns #-}

module Battle (module Battle) where

import Card
import qualified Data.Set as Set

data AttackAction = Attack Card | EndAttack
  deriving (Show, Ord, Eq)

data DefendAction = Defend Card | GiveUp
  deriving (Show, Ord, Eq)

data Action = A AttackAction | D DefendAction deriving (Show, Eq)

data Battle = Battle Card (Maybe Card) deriving (Eq)

type Battles = [Battle]

instance Show Battle where
  show (Battle a d) = show a ++ maybe "?" (('/' :) . show) d

getCards :: Battles -> Cards
getCards [] = []
getCards (Battle a Nothing : bs) = a : getCards bs
getCards (Battle a (Just d) : bs) = a : d : getCards bs

getRanks :: Battles -> [Rank]
getRanks bs = Set.toList $ Set.fromList (map rank (getCards bs))

-- Sorting the attack & defend actions by their value - useful in determining the action's severity
cmpAttacks :: Trump -> AttackAction -> AttackAction -> Ordering
cmpAttacks _ EndAttack EndAttack = EQ
cmpAttacks _ EndAttack _ = GT
cmpAttacks _ _ EndAttack = LT
cmpAttacks t (Attack c) (Attack c') = cardOrder t c c'

cmpDefends :: Trump -> DefendAction -> DefendAction -> Ordering
cmpDefends _ GiveUp GiveUp = EQ
cmpDefends _ GiveUp _ = GT
cmpDefends _ _ GiveUp = LT
cmpDefends t (Defend c) (Defend c') = cardOrder t c c'
