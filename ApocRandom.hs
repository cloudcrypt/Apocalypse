{- |

-}
module ApocRandom (
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
                else return (Just (blah (moves !! ranNum)))
randomChoice g PawnPlacement p = do
                let cells = emptyCells g
                ranNum <- (randomRIO (0, ((length cells) - 1)))
                return (Just [(cells !! ranNum)])

-- | Sets the values a small array of two tuples so it can be taken by the function to return a random

blah :: Played -> [(Int,Int)]
blah (Played (src,dst)) = [src,dst]

-- | Gets the empty cells so that the pawn placement can happen in a random location

emptyCells :: GameState -> [(Int,Int)]
emptyCells g = filter (\x -> (getFromBoard (theBoard g) x)==E) cells                    
