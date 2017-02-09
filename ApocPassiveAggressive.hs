--ApocPassiveAggressive.hs

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

{- |This method is used to convert a Played type into a touple
-}
blah :: Played -> [(Int,Int)]
blah (Played (src,dst)) = [src,dst]


{- | This function will look through the defined board and will check if the value is
an empty character. If the character is empty it will return a boolean true.
-}
killMove :: GameState -> Played -> Bool
killMove gs (Played (src, dst)) = (getFromBoard (theBoard gs) dst) /= E








