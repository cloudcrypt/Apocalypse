--ApocPassiveAggressive.hs

module ApocPassiveAggressive where

import ApocTools
import ApocUtility
import ApocStrategyGreedy

passAgg :: Chooser
passAgg gs Normal p = do
        let moves = (filter (killMove gs) (validMoves p board))
		in case (length moves) of
                0 -> return $ Just Nothing
                _ -> return $ Just moves !! 0 



killMove :: GameState -> Played -> Bool
killMove gs (Played (src, dst)) = (getFromBoard (theBoard gs) dst) /= E


		
--passAgg gs Played =  

{- | 
if kill move possible
then execute
else pass on turn
-}

{-
getMoves :: Player -> [[a]] -> [(Int, Int)] -> [((Int, Int),(Int, Int))]
getMoves player board ((x,y):xs)
		| getBoardVal board (x,y) == BK = [((),())]  --how to do the +1,+2
		| getBoardVal board (x,y) == BP = [((x,y),(x,y+1))]
		| getBoardVal board (x,y) == WK = [((),())]
		| getBoardVal board (x,y) == WP = [((x,y),(x,y-1))]
-}

--killMove :: Player -> [[cell]] -> [((Int, Int),(Int,Int))]
--killMoves :: [Played] -> board
--killMoves (((x,y),(p,q)):xs)
	--	| getFromBoard board (p,q) == BK = [((x,y),(p,q))]
	--	| getFromBoard board (p,q) == BP = [((x,y),(p,q))]
	--	| getFromBoard board (p,q) == WK = [((x,y),(p,q))]
	--	| getFromBoard board (p,q) == WP = [((x,y),(p,q))]
	--	|otherwise []
		 


-- | Return the value (BK, BP etc.) at a point from the board
--it takes in a 2d array and (x,y) and will return what is at that location
--it will find the index of the xs and then find the index of the 

--snd = (a,b) the b
--fst = (a,b) the a

--take the second of the touple and index it, then take the first of the touple and index that
--move to apocTools
--getBoardVal             :: [[a]] -> (Int,Int) -> a
--getBoardVal xs pt       = xs !! fst pt !! snd pt


