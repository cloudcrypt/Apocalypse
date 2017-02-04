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
import ApocUtility
import ApocStrategies
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
  case (length args) of 
    2 -> do
      let strats = validateStrategies args
      case strats of 
        Nothing -> displayStrategies
        _ -> processTurn initBoard (fromJust strats) -- here (fromJust strats) equals a valid (Chooser(Black),Chooser(White))
    0 -> do
      displayStrategies
      stratStrings <- getStrategies
      let strats = validateStrategies stratStrings
      case strats of 
        Nothing -> displayStrategies
        _ -> processTurn initBoard (fromJust strats) -- here (fromJust strats) equals a valid (Chooser(Black),Chooser(White))
    _ -> do
      displayStrategies

processTurn     :: GameState -> ((String,Chooser),(String,Chooser)) -> IO ()
processTurn g (bChooser,wChooser) = do
  putStrLn (show g)
  case (gameOverCheck g (fst wChooser) (fst bChooser)) of
    Just str -> do putStrLn str
    Nothing -> do
      black <- (snd bChooser) g Normal Black
      white <- (snd wChooser) g Normal White
      newState <- verifyPawnUpgrade $ performMoves white black g
      processTurn newState (bChooser,wChooser)

verifyPawnUpgrade  :: GameState -> IO GameState
verifyPawnUpgrade g = do
  state <- processPawnUpgrade (whitePlay g) White g
  processPawnUpgrade (blackPlay g) Black state

processPawnUpgrade :: Played -> Player -> GameState -> IO GameState
processPawnUpgrade (Played move) player g
  | (upgradeableMove (Played move) player g) = do
    putStrLn (show g)
    case (pieceCount (theBoard g) player Knight)<2 of
      True -> return $ performPawnUpgrade player (Played move) g
      False -> do
        placeMove <- human g PawnPlacement player
        return $ performPawnPlace placeMove (snd move) player g
  | otherwise = do
    return g
processPawnUpgrade _ _ g = do return g

performPawnUpgrade :: Player -> Played -> GameState -> GameState
performPawnUpgrade White (Played (_,(x2,y2))) g = let wPlay = UpgradedPawn2Knight (x2,y2)
                                                      bPlay = None
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications wPlay bPlay g)) g
performPawnUpgrade Black (Played (_,(x2,y2))) g = let wPlay = None
                                                      bPlay = UpgradedPawn2Knight (x2,y2)
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications bPlay wPlay g)) g

performPawnPlace  :: Maybe [(Int,Int)] -> (Int,Int) -> Player -> GameState -> GameState
performPawnPlace move src p g = modifyGameState (verifyPawnPlace move src p g) g

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
                                (addModifications wPlay bPlay g))

verifyPawnPlace  :: Maybe [(Int,Int)] -> (Int,Int) -> Player -> GameState -> ((Played, Int), (Played, Int), [BoardModification])
verifyPawnPlace move src p g = let (play, penalty) = if move==Nothing
                                                     then (NullPlacedPawn, 0)
                                                     else (verifyPawnPlaceLegality ([src] ++ (fromJust move)) g)   
                               in case p of
                                    White -> ((play, penalty),
                                              (None, 0),
                                              (addModifications play None g))
                                    Black -> ((None, 0),
                                              (play, penalty),
                                              (addModifications play None g))                 

addModifications     :: Played -> Played -> GameState -> [BoardModification]
addModifications (Played ((ax1,ay1),(ax2,ay2))) (Played ((bx1,by1),(bx2,by2))) g = let cellA = getFromBoard (theBoard g) (ax1,ay1)
                                                                                       cellB = getFromBoard (theBoard g) (bx1,by1)
                                                                                       typeA = pieceTypeOf cellA
                                                                                       typeB = pieceTypeOf cellB
                                                                                   in [Delete (ax1,ay1)] ++ [Delete (bx1,by1)]
                                                                                      ++ case (((ax2,ay2)==(bx2,by2)),(typeA==typeB),typeA) of
                                                                                          (False,_,_) -> [Place cellA (ax2,ay2)] ++ [Place cellB (bx2,by2)]
                                                                                          (True,True,_) -> []
                                                                                          (True,False,Knight) -> [Place cellA (ax2,ay2)]
                                                                                          (True,False,Pawn) -> [Place cellB (bx2,by2)]
addModifications (Played ((ax1,ay1),(ax2,ay2))) _ g = [Move (ax1,ay1) (ax2,ay2)]
addModifications _ (Played ((bx1,by1),(bx2,by2))) g = [Move (bx1,by1) (bx2,by2)] 
addModifications (UpgradedPawn2Knight (x,y)) None g = let player = playerOf (pieceOf (getFromBoard (theBoard g) (x,y)))
                                                      in case player of
                                                        White -> [Place WK (x,y)]
                                                        Black -> [Place BK (x,y)]
addModifications (PlacedPawn ((x1,y1),(x2,y2))) None g = let player = playerOf (pieceOf (getFromBoard (theBoard g) (x1,y1)))
                                                      in [Delete (x1,y1)]
                                                          ++ case player of
                                                              White -> [Place WP (x2,y2)]
                                                              Black -> [Place BP (x2,y2)]                                                                                                             
addModifications _ _ g = []

verifyMoveLegality  :: [(Int, Int)] -> Player -> GameState -> (Played, Int)
verifyMoveLegality move p g = let (x1,y1) = (move !! 0)
                                  (x2,y2) = (move !! 1)
                                  cell1 = getFromBoard (theBoard g) (x1,y1)
                                  cell2 = getFromBoard (theBoard g) (x2,y2)
                                  cell1Player = playerOf (pieceOf cell1)
                              in case ((cell1==E) || (cell1Player/=p) || (samePlayer cell1 cell2)) of
                                True -> ((Goofed ((x1,y1), (x2,y2))), 1)
                                False -> if (verifyPieceDest (pieceTypeOf cell1) cell1 cell2 (x1,y1) (x2,y2))
                                         then ((Played ((x1,y1), (x2,y2))), 0)
                                         else ((Goofed ((x1,y1), (x2,y2))), 1)

verifyPawnPlaceLegality :: [(Int, Int)] -> GameState -> (Played, Int)
verifyPawnPlaceLegality (xy1:xy2:_) g = case (getFromBoard (theBoard g) xy2) of
                                    E -> ((PlacedPawn (xy1,xy2)), 0)
                                    _ -> ((BadPlacedPawn (xy1,xy2)), 1)

verifyPieceDest   :: PieceType -> Cell -> Cell -> (Int, Int) -> (Int, Int) -> Bool
verifyPieceDest Knight _ dstCell (x1,y1) (x2,y2) = let columnDiff = abs (x2 - x1)
                                                       rowDiff = abs (y2 - y1)
                                                   in (((columnDiff==1) && (rowDiff==2)) || ((columnDiff==2) && (rowDiff==1)))                                   
verifyPieceDest Pawn WP dstCell (x1,y1) (x2,y2) = if y2==(y1+1)
                                                  then if x2==x1
                                                       then True
                                                       else (((abs (x2-x1))==1) && ((dstCell==BP) || (dstCell==BK)))
                                                  else False
verifyPieceDest Pawn BP dstCell (x1,y1) (x2,y2) = if y2==(y1-1)
                                                  then if x2==x1
                                                       then True
                                                       else (((abs (x2-x1))==1) && ((dstCell==WP) || (dstCell==WK)))
                                                  else False                                                                             

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

gameOverCheck :: GameState -> String -> String -> Maybe String
gameOverCheck g wChooserStr bChooserStr
    | wPawns==0 && bPawns==0 = Just (gameOverString "Draw!" wPawns bPawns wChooserStr bChooserStr)
    | wPawns==0 || (whitePen g)==2 = Just (gameOverString "Black wins!" wPawns bPawns wChooserStr bChooserStr)
    | bPawns==0 || (blackPen g)==2 = Just (gameOverString "White wins!" wPawns bPawns wChooserStr bChooserStr)
    | (whitePlay g)==Passed && (blackPlay g)==Passed = case (wPawns==bPawns,wPawns>bPawns) of
                                                        (True,_) -> Just (gameOverString "Draw!" wPawns bPawns wChooserStr bChooserStr)
                                                        (False,True) -> Just (gameOverString "White wins!" wPawns bPawns wChooserStr bChooserStr)
                                                        _ -> Just (gameOverString "Black wins!" wPawns bPawns wChooserStr bChooserStr)
    | otherwise = Nothing
    where wPawns = pieceCount (theBoard g) White Pawn
          bPawns = pieceCount (theBoard g) Black Pawn

gameOverString :: String -> Int -> Int -> String -> String -> String
gameOverString winner wPawns bPawns wChooserStr bChooserStr = winner ++ "   Black (" ++ bChooserStr ++ "): " ++ (show bPawns) ++ "  White (" ++ wChooserStr ++ "): " ++ (show wPawns)

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