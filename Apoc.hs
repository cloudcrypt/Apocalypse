{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO ()
main' args = do
    -- check if args is anything
    -- if so:
    putStrLn "Possible strategies:\n  human\n"
    -- print all strategies
    processTurn initBoard

processTurn     :: GameState -> IO ()
processTurn g = do
  -- check end game
  putStrLn (show g)
  black <- human (g) Normal Black
  white <- whiteHuman (g) Normal White
  -- processTurn $ performMoves black white g
  putStrLn (show $ performMoves black white g)

-- check stuff
-- resolve issues
-- modify black and white moves (coordinates accordingly)
-- then call modifyGameState twice as we are doing now
performMoves    :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState -> GameState
performMoves black white g = modifyGameState black (modifyGameState white g)

modifyGameState :: Maybe [(Int,Int)] -> GameState -> GameState
modifyGameState move g = GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen g)
                               (Passed)
                               (whitePen g)
                               (replace2 (replace2 (theBoard g)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard g) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E)

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

