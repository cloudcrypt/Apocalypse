{- |
Module: ApocPassiveAggressive.hs
Description: Cpsc449 W2017 - Group 24 
Copyright: (c) Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License: None (Assignment)
Portability: ghc 7.10.3 , Needs Cabal
-}

{- |
This class is a passive aggressive strategy. The AI will pass if there are no available kill moves within 
the reach of its pieces. It will calculate all the possible moves at the start and will filter out the moves
that will be kill moves. It will then check if there are kill moves. If not it will pass.
-}
module ApocStrategyPassiveAggressive(
    passAgg
    ) where

import ApocTools
import ApocUtility
import System.Random


{- |This method will first filter out the killmoves and valid moves from the user
sepcified board. It will then check the length of the list and if the list is empty
(length 0) nothing is returned. If there is something, it will convert the moves to 
a touple and will return. For pawnplacement, nothing is returned.
-}
passAgg :: Chooser
passAgg gs Normal p = let moves = (filter (killMove gs) (validMoves p gs))
                      in case (length moves) of
                        0 -> do return $ Nothing
                        _ -> do
                            ranNum <- randomRIO(0, ((length moves) -1))
                            return $ Just (playedToMove (moves !! ranNum))
                           
passAgg gs PawnPlacement p = do -- Find all of the empty cells on the board and randomly place the pawn in one of the locations
                let cells = emptyCells g
                ranNum2 <- (randomRIO (0, ((length cells) - 1)))
                return (Just [(cells !! ranNum)])

killMove :: GameState -> Played -> Bool --This function will look through the defined board and will check if the value is an empty character. 
killMove gs (Played (src, dst)) = (getFromBoard (theBoard gs) dst) /= E --If the character is empty it will return a boolean true.



-- | Gets the empty cells so that the pawn placement can happen in a random location
emptyCells :: GameState -> [(Int,Int)]
emptyCells g = filter (\x -> (getFromBoard (theBoard g) x)==E) cells





