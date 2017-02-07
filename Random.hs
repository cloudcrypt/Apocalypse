
module ApocStrategies (
        randomChoice
        ) where

import ApocUtility
import System.Random

randomChoice :: Chooser
randomChoice g Normal p = do
                ranNum <- (randomRIO (-1, ((length (validMoves p g)) - 1))
                if ranNum == -1
                        then return Nothing
                        else return just((validMoves p g)!!ranNum)
randomChoice g PawnPlacement p = do
                ranPlace <-(randomRIO(0, (length (ifEmpty 0 0 [] g)))-1))
                return just ((ifEmpty 0 0 [] g)!!ranPlace)

 
ifEmtpy :: Int -> Int -> [(Int,Int)] -> GameState -> [(Int, Int)]
ifEmpty x 5 z g = ifEmpty (x + 1) 0 z g
ifEmpty 5 y z g = z
ifEmpty x y z g | getBoardVal (theBoard g) (x,y) == E = (x,y):z ifEmpty x (y + 1) z g
                | otherwise = ifEmpty x (y + 1) z g
