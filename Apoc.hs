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
  white <- whiteHuman (g) Normal White
  black <- blackHuman (g) Normal Black
  processTurn2 $ performMoves white black g
  --putStrLn (show $ performMoves white black g)

processTurn2     :: GameState -> IO ()
processTurn2 g = do
  -- check end game
  putStrLn (show g)
  white <- whiteHuman2 (g) Normal White
  black <- blackHuman2 (g) Normal Black
  --processTurn2 $ performMoves white black g
  putStrLn (show $ performMoves white black g)

-- check stuff
-- resolve issues
-- modify black and white moves (coordinates accordingly)
-- then call modifyGameState twice as we are doing now
performMoves    :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState -> GameState
performMoves white black g = modifyGameState (verifyMoves white black g) g

verifyMoves     :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState -> ((Played, Int), (Played, Int), [BoardModification])
verifyMoves white black g = let (wPlay, wPenalty) = if white==Nothing 
                                                    then (Passed, 0)
                                                    else (verifyMoveLegality (fromJust white) White g)
                                (bPlay, bPenalty) = if black==Nothing
                                                    then (Passed, 0)
                                                    else (verifyMoveLegality (fromJust black) Black g)
                            in ((wPlay, wPenalty),
                                (bPlay, bPenalty),
                                (addModifications wPlay bPlay [] g))

addModifications     :: Played -> Played -> [BoardModification] -> GameState -> [BoardModification]
addModifications (Played ((ax1,ay1),(ax2,ay2))) (Played ((bx1,by1),(bx2,by2))) mods g = let cellA = getFromBoard (theBoard g) (ax1,ay1)
                                                                                            cellB = getFromBoard (theBoard g) (bx1,by1)
                                                                                        in if (((ax2,ay2)==(bx2,by2)) && (cellA==cellB))
                                                                                            then mods ++ [Delete (ax1,ay1)] ++ [Delete (bx1,by1)]
                                                                                            else mods ++ [Delete (ax1,ay1)] ++ [Delete (bx1,by1)] 
                                                                                                      ++ if ((ax2,ay2)==(bx2,by2))
                                                                                                         then if ((pieceTypeOf cellA)==Knight)
                                                                                                              then [Place cellA (ax2,ay2)]
                                                                                                              else [Place cellB (bx2,by2)]
                                                                                                         else [Place cellA (ax2,ay2)] ++ [Place cellB (bx2,by2)]
-- addModifications (Played ((ax1,ay1),(ax2,ay2))) (Played ((bx1,by1),(bx2,by2))) mods g = mods ++ [Move (ax1,ay1) (ax2,ay2)] ++ [Move (bx1,by1) (bx2,by2)]
addModifications (Played ((ax1,ay1),(ax2,ay2))) (Goofed ((bx1,by1),(bx2,by2))) mods g = mods ++ [Move (ax1,ay1) (ax2,ay2)]
addModifications (Goofed ((ax1,ay1),(ax2,ay2))) (Played ((bx1,by1),(bx2,by2))) mods g = mods ++ [Move (bx1,by1) (bx2,by2)]
addModifications _ _ mods g = mods

-- addModification wPlay bPlay mods g = if ((wPlay!=Nothing) && (bPlay!=Nothing))
--                                      then if ((wPlay!=Goofed) && (bPlay!=Goofed))
--                                           then if 
--                                           else if (wPlay!=Nothing)
--                                      else if (wPlay!=Nothing)

-- getCell             :: Played -> GameState -> Cell
-- getCell play g = 

verifyMoveLegality  :: [(Int, Int)] -> Player -> GameState -> (Played, Int)
verifyMoveLegality move p g = let (x1,y1) = (move !! 0)
                                  (x2,y2) = (move !! 1)
                                  cell1 = getFromBoard (theBoard g) (x1,y1)
                                  cell2 = getFromBoard (theBoard g) (x2,y2)
                              in if ((cell1==E) || ((playerOf (pieceOf cell1))/=p))
                                 then ((Goofed ((x1,y1), (x2,y2))), 1)
                                 else if cell1==cell2
                                      then ((Goofed ((x1,y1), (x2,y2))), 1)
                                      else if (verifyPieceDest (pieceTypeOf cell1) cell1 cell2 (x1,y1) (x2,y2))
                                           then ((Played ((x1,y1), (x2,y2))), 0)
                                           else ((Goofed ((x1,y1), (x2,y2))), 1)  

verifyPieceDest    :: PieceType -> Cell -> Cell -> (Int, Int) -> (Int, Int) -> Bool
verifyPieceDest Knight _ dstCell (x1,y1) (x2,y2) = let columnDiff = abs (x2 - x1)
                                                       rowDiff = abs (y2 - y1)
                                                   in (((columnDiff==1) && (rowDiff==2)) || ((columnDiff==2) && (rowDiff==1)))                                   
verifyPieceDest Pawn WP dstCell (x1,y1) (x2,y2) = if (y2==(y1+1))
                                                  then if (x2==x1)
                                                       then True
                                                       else (((abs (x2-x1))==1) && ((dstCell==BP) || (dstCell==BK)))
                                                  else False
verifyPieceDest Pawn BP dstCell (x1,y1) (x2,y2) = if (y2==(y1-1))
                                                  then if (x2==x1)
                                                       then True
                                                       else (((abs (x2-x1))==1) && ((dstCell==WP) || (dstCell==WK)))
                                                  else False                                                                             

data PieceType  = Pawn | Knight deriving (Eq)
-- | Given a 'Cell', return the corresponding 'Piece'.
pieceTypeOf     :: Cell -> PieceType
pieceTypeOf BK = Knight
pieceTypeOf WK = Knight
pieceTypeOf BP = Pawn
pieceTypeOf WP = Pawn

modifyGameState :: ((Played, Int), (Played, Int), [BoardModification]) -> GameState -> GameState
modifyGameState ((wPlay, wPenalty), (bPlay, bPenalty), mods) g = 
  GameState (bPlay)
         ((blackPen g) + bPenalty)
         (wPlay)
         ((whitePen g) + wPenalty)
         (applyBoardModifications mods (theBoard g))

data BoardModification = Move (Int, Int) (Int, Int)
                       | Delete (Int, Int)
                       | Place Cell (Int, Int)

applyBoardModifications   :: [BoardModification] -> Board -> Board
applyBoardModifications [] b = b
applyBoardModifications (x:xs) b = applyBoardModifications xs (modifyBoard x b)

modifyBoard   :: BoardModification -> Board -> Board
modifyBoard (Move (x1,y1) (x2,y2)) b = (replace2 (replace2 b
                                                   (x2,y2)
                                                   (getFromBoard b (x1,y1)))
                                         (x1,y1)
                                         E)
modifyBoard (Delete (x,y)) b = (replace2 b
                                         (x,y)
                                         E)
modifyBoard (Place c (x,y)) b = (replace2 b
                                         (x,y)
                                         c)

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

