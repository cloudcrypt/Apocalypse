
module ApocRandom (
        randomChoice
        ) where

import ApocUtility
import ApocTools
import System.Random

-- |This generates random numbers.

randomChoice :: Chooser
randomChoice g Normal p = do
                let moves = validMoves p g
                ranNum <- (randomRIO (-1, ((length moves) - 1)))
                if ranNum == -1
                then return Nothing
                else return (Just (blah (moves !! ranNum)))
randomChoice g PawnPlacement p = do
                let cells = emptyCells g
                ranNum <- (randomRIO (0, ((length cells) - 1)))
                return (Just [(cells !! ranNum)])
                -- ranPlace <- (randomRIO(0, (length (ifEmpty 0 0 [] g)))-1)
                -- return Just ((ifEmpty 0 0 [] g) !! ranPlace)


blah :: Played -> [(Int,Int)]
blah (Played (src,dst)) = [src,dst]
 
-- ifEmpty :: Int -> Int -> [(Int,Int)] -> GameState -> [(Int, Int)]
-- ifEmpty x 5 z g = ifEmpty (x + 1) 0 z g
-- ifEmpty 5 y z g = z
-- ifEmpty x y z g | getBoardVal (theBoard g) (x,y) == E = (x,y):z ifEmpty x (y + 1) z g
--                 | otherwise = ifEmpty x (y + 1) z g

emptyCells :: GameState -> [(Int,Int)]
emptyCells g = filter (\x -> (getFromBoard (theBoard g) x)==E) cells                    
