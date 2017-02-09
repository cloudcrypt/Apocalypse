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
module ApocPassiveAggressive(
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
passAgg gs Normal p = do
    let moves = (filter (killMove gs) (validMoves p gs))
    let ranNum <- randomRIO(0, ((length moves) -1))
	in case (length moves) of
        0 -> return $ Nothing
        _ -> return $ Just (blah (moves !! ranNum))
passAgg gs PawnPlacement p = return Nothing


blah :: Played -> [(Int,Int)] -- ^This method is used to convert a Played type into a touple
blah (Played (src,dst)) = [src,dst] 


killMove :: GameState -> Played -> Bool --This function will look through the defined board and will check if the value is an empty character. 
killMove gs (Played (src, dst)) = (getFromBoard (theBoard gs) dst) /= E --If the character is empty it will return a boolean true.








