{- |
Module: ApocRandom.hs
Description: Cpsc449 W2017 - Group 24 
Copyright: (c) Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License: None (Assignment)
Portability: ghc 7.10.3 , Needs Cabal
This is a strategy taht generates a completely random move.
-}


module ApocStrategyRandom (
        randomChoice
        ) where

import ApocUtility
import ApocTools
import System.Random

-- |This generates a random decesion which choose some moves or a pass 

randomChoice :: Chooser
randomChoice g Normal p = do
                let moves = validMoves p g
                ranNum <- (randomRIO (-1, ((length moves) - 1)))
                if ranNum == -1
                then return Nothing
                else return (Just (playedToMove (moves !! ranNum)))
randomChoice g PawnPlacement p = do
                let cells = emptyCells g
                ranNum <- (randomRIO (0, ((length cells) - 1)))
                return (Just [(cells !! ranNum)])
