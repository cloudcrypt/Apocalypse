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
      --replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocUtility
import ApocStrategies


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
      newState <- verifyPawnUpgrade (performMoves white black g) (snd wChooser) (snd bChooser)
      processTurn newState (bChooser,wChooser)

verifyPawnUpgrade  :: GameState -> Chooser -> Chooser -> IO GameState
verifyPawnUpgrade g wChooser bChooser = do
  state <- processPawnUpgrade (whitePlay g) White g wChooser bChooser
  processPawnUpgrade (blackPlay g) Black state wChooser bChooser

processPawnUpgrade :: Played -> Player -> GameState -> Chooser -> Chooser -> IO GameState
processPawnUpgrade (Played move) player g wChooser bChooser
  | (upgradeableMove (Played move) player g) = do
    putStrLn (show g)
    case (pieceCount (theBoard g) player Knight)<2 of
      True -> return $ performPawnUpgrade player (Played move) g
      False -> do
        placeMove <- (if player==White then wChooser else bChooser) g PawnPlacement player
        return $ performPawnPlace placeMove (snd move) player g
  | otherwise = do
    return g
processPawnUpgrade _ _ g _ _ = do return g


{- |This function will take in a player, a move and a gamestate. It will process the white and black players separately. It will 
call the upgradePawn2Knight method on the specified location and will then return the new modified gamestate.
-}
performPawnUpgrade :: Player -> Played -> GameState -> GameState
performPawnUpgrade White (Played (_,(x2,y2))) g = let wPlay = UpgradedPawn2Knight (x2,y2)
                                                      bPlay = None
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications wPlay bPlay g)) g
performPawnUpgrade Black (Played (_,(x2,y2))) g = let wPlay = None
                                                      bPlay = UpgradedPawn2Knight (x2,y2)
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications bPlay wPlay g)) g


{- |This function will take in a src and destination of a move along with the player and gamestate. It will then call the 
modifyGameState function after it verifies the move location.
-}
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

verifyPawnPlaceLegality :: [(Int, Int)] -> GameState -> (Played, Int)
verifyPawnPlaceLegality (xy1:xy2:_) g = case (getFromBoard (theBoard g) xy2) of
                                    E -> ((PlacedPawn (xy1,xy2)), 0)
                                    _ -> ((BadPlacedPawn (xy1,xy2)), 1)                                                                           


{- |This function will check if the end game state has been reached. It will first check if there is a draw by the number of
pawns of both teams ==0. Then it will check either white or black for penalties and then compare the number of pawns of the
black and white teams and if it is zero, it will call the gameoverstring method.
-}
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


-- |This method will display the end game string along with the strategies chosen and number of pawns remaining.
gameOverString :: String -> Int -> Int -> String -> String -> String
gameOverString winner wPawns bPawns wChooserStr bChooserStr = winner ++ "   Black (" ++ bChooserStr ++ "): " ++ (show bPawns) ++ "  White (" ++ wChooserStr ++ "): " ++ (show wPawns)
