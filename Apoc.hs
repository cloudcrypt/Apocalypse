{- |
Module      : Main
Description : Main module of Apocalypse game.
Copyright   : (c) 2017 Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License     : None
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 8.0.2

This module is the main entry point for the Apocalypse game, where input is taken
in from the user, and turns are processed until the end game state is reached.
-}

module Main (
      -- * Main
      main, main',
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

{- | 
     The main function validates the number of command line arguments, and will either parse
     them or get input from the user and parse it, starting the game using the parsed data, or
     print the list of strategies when the arguments are invalid.
     main' can be called either by calling the program from GHCi in the usual way, or by running
     the program from the command line by calling this function with the value from (getArgs).
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

{- |
    Processes a turn, first checking for the game over state, and then getting 
    a move from a black and white strategy. After performing a move, checks game
    over state before verifying pawn upgrade state and recursing.
-}
processTurn     :: GameState -> ((String,Chooser),(String,Chooser)) -> IO ()
processTurn g (bChooser,wChooser) = do
  putStrLn (show g)
  case (gameOverCheck g (fst wChooser) (fst bChooser)) of
    Just str -> do putStrLn str
    Nothing -> do
      black <- (snd bChooser) g Normal Black
      white <- (snd wChooser) g Normal White
      let newState = performMoves white black g
      case (gameOverCheck newState (fst wChooser) (fst bChooser)) of
        Just str -> do 
          putStrLn (show newState)
          putStrLn str
        Nothing -> do
          verifiedState <- verifyPawnUpgrade newState (snd wChooser) (snd bChooser)
          processTurn verifiedState (bChooser,wChooser)

{- |
    Verifies the pawn upgrade state by processing a pawn upgrade for both black
    and white players.
-}
verifyPawnUpgrade  :: GameState -> Chooser -> Chooser -> IO GameState
verifyPawnUpgrade g wChooser bChooser = do
  state <- processPawnUpgrade (whitePlay g) White g wChooser bChooser
  processPawnUpgrade (blackPlay g) Black state wChooser bChooser

{- |
    If a player has performed an upgradeable move, processes a pawn upgrade
    by either upgrading a pawn if number of player's knights < 2, or getting a
    pawn placement move from the player's strategy and performing it, if the
    number of player's knights == 2.
-}
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

{- |
    Upgrades a pawn to a knight for a specific player at a specific coordinate
    by modifying the game state with a UpgradedPawn2Knight (x,y) move for that player
    at that coordinate.
-}
performPawnUpgrade :: Player -> Played -> GameState -> GameState
performPawnUpgrade White (Played (_,(x2,y2))) g = let wPlay = UpgradedPawn2Knight (x2,y2)
                                                      bPlay = None
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications wPlay bPlay g)) g
performPawnUpgrade Black (Played (_,(x2,y2))) g = let wPlay = None
                                                      bPlay = UpgradedPawn2Knight (x2,y2)
                                                  in modifyGameState ((wPlay,0),(bPlay,0),(addModifications bPlay wPlay g)) g

{- |
    Verifies a pawn placement move given a move and src location in verifyPawnPlace,
    and then passes verified tuple from verifyPawnPlace to modifyGameState to perform
    the placement.
-}
performPawnPlace  :: Maybe [(Int,Int)] -> (Int,Int) -> Player -> GameState -> GameState
performPawnPlace move src p g = modifyGameState (verifyPawnPlace move src p g) g

{- |
    Verifies a normal move given a white and black move in verifyMoves,
    and then passes verified tuple from verifyPawnPlace to modifyGameState to perform
    the moves.
-}
performMoves    :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState -> GameState
performMoves white black g = modifyGameState (verifyMoves white black g) g

{- |
    Verifies a normal white and black move, generating a tuple that can be passed to
    modifyGameState. To generate the tuple, checks moves, assigning Passed and penalty
    of 0 if Nothing, or a (Played,Int) tuple from verifyMoveLegality otherwise, and generates
    a list of board modifications based on the verified white and black moves.
-}
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

{- |
    Verifies a pawn placement move, generating a tuple that can be passed to
    modifyGameState. To generate the tuple, checks move, assigning NullPlacedPawn and penalty
    of 1 if Nothing, or a (Played,Int) tuple from verifyPawnPlaceLegality otherwise, and generates
    a list of board modifications based on the verified placement move.
-}
verifyPawnPlace  :: Maybe [(Int,Int)] -> (Int,Int) -> Player -> GameState -> ((Played, Int), (Played, Int), [BoardModification])
verifyPawnPlace move src p g = let (play, penalty) = if move==Nothing
                                                     then (NullPlacedPawn, 1)
                                                     else (verifyPawnPlaceLegality ([src] ++ (fromJust move)) g)   
                               in case p of
                                    White -> ((play, penalty),
                                              (None, 0),
                                              (addModifications play None g))
                                    Black -> ((None, 0),
                                              (play, penalty),
                                              (addModifications play None g))                 

{- |
    Verifies a the legality of a pawn placement move, from src cell xy1
    to dst cell xy2. If move is valid, outputs a Played of PlacedPawn and penalty
    of 0. If move is invalid, outputs a Played of BadPlacedPawn and a penalty of 1.
-}
verifyPawnPlaceLegality :: [(Int, Int)] -> GameState -> (Played, Int)
verifyPawnPlaceLegality (xy1:xy2:_) g = case (getFromBoard (theBoard g) xy2) of
                                    E -> ((PlacedPawn (xy1,xy2)), 0)
                                    _ -> ((BadPlacedPawn (xy1,xy2)), 1)                                                                           


{- |
    Checks if the end game state has been reached. It will first check if there is a draw if the number of
    pawns of both players is 0. Then it will check either white or black for penalty == 2 and then compare the number of pawns of the
    black and white teams to 0. Then it will check if both players have passed in the same turn, and compare the number of pawns
    for the players. If there is a draw between the players, or if a player has penalty == 2 or number of pawns == 0, or if both
    players passed in the last turn, it will then then generate through gameOverString and output a generated game over string.
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


-- |This method will generate an end game string along with the strategies chosen and number of pawns remaining for both players.
gameOverString :: String -> Int -> Int -> String -> String -> String
gameOverString winner wPawns bPawns wChooserStr bChooserStr = winner ++ "   Black (" ++ bChooserStr ++ "): " ++ (show bPawns) ++ "  White (" ++ wChooserStr ++ "): " ++ (show wPawns)
