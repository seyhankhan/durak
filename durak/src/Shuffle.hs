module Shuffle (module Shuffle) where

import System.Random (StdGen, randomR)

-- https://literateprograms.org/fisher-yates_shuffle__haskell_.html
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle l gen = shuffle' l gen []
 where
  shuffle' [] g acc = (acc, g)
  shuffle' l g acc =
    let
      (k, g') = randomR (0, length l - 1) g
      (lead, x : xs) = splitAt k l
     in
      shuffle' (lead ++ xs) g' (x : acc)