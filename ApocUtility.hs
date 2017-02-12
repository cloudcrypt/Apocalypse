{- |
Module      : ApocUtility
Description : Utility functions needed for Apocalypse game.
Copyright   : (c) 2017 Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License     : None
Stability   : experimental
Portability : ghc 7.10.2 - 8.0.2, requires System.Random
-}
module ApocUtility (
    PieceType(Pawn,Knight),
    pieceTypeOf,
    samePlayer,
    otherPlayer,
    upgradeableMove,
    pieceCount,
    isIdentical,
    fst4,
    snd4,
    thd4,
    frt4,
    playedToMove,
    placedPawnToMove,
    verifyMoveLegality,
    addModifications,
    BoardModification(Move,Delete,Place),
    modifyGameState,
    noValidMoves,
    validMoves,
    validPlacements,
    cells,
    emptyCells
    ) where

import ApocTools
import Data.Char (isDigit, digitToInt)

-- | The possible types of pieces in the game board.
data PieceType  = Pawn    -- ^ A piece of type Pawn
                | Knight  -- ^ A piece of type Knight
                deriving (Eq)

-- | Given a 'Cell', return the corresponding 'Piece'.
pieceTypeOf     :: Cell -> PieceType
pieceTypeOf BK  = Knight
pieceTypeOf WK  = Knight
pieceTypeOf BP  = Pawn
pieceTypeOf WP  = Pawn 

-- | Outputs true if two cells contain pieces belonging to the same player, false otherwise.
samePlayer              :: Cell -> Cell -> Bool
samePlayer cell1 cell2  = case ((cell1==E) || (cell2==E)) of
                          True -> False
                          False -> (playerOf (pieceOf cell1))==(playerOf (pieceOf cell2))

-- | Given a player, outputs the opposite player.
otherPlayer     :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White                  

-- | Outputs true if the last Played move of a player has left a pawn in an upgradeable state,
-- and false otherwise.
upgradeableMove :: Played -> Player -> GameState -> Bool
upgradeableMove (Played (_,(x2,y2))) White g = ((getFromBoard (theBoard g) (x2,y2))==WP && y2==4)
upgradeableMove (Played (_,(x2,y2))) Black g = ((getFromBoard (theBoard g) (x2,y2))==BP && y2==0)

-- | Outputs the number of a specific PieceType on the board for a specific Player.
pieceCount     :: Board -> Player -> PieceType -> Int
pieceCount [] _ _ = 0
pieceCount (x:xs) p pt = (pieceCountRow x p pt) + (pieceCount xs p pt)

-- | Calculates the number of a specific PieceType in a row of the board, and outputs 
-- that as an int.
pieceCountRow  :: [Cell] -> Player -> PieceType -> Int
pieceCountRow [] _ _ = 0
pieceCountRow (E:xs) p pt = pieceCountRow xs p pt
pieceCountRow (x:xs) p pt = if ((playerOf (pieceOf x))==p && (pieceTypeOf x)==pt)
                            then 1 + (pieceCountRow xs p pt)
                            else pieceCountRow xs p pt 

-- | Outputs true if every element of an array is equal to some element, and false otherwise.
isIdentical :: Eq a => a -> [a] -> Bool
isIdentical a [] = True
isIdentical a (x:xs) = case a==x of
                        True -> isIdentical a xs
                        False -> False                   

-- | Outputs the first element in a 4-tuple.
fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

-- | Outputs the second element in a 4-tuple.
snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

-- | Outputs the third element in a 4-tuple.
thd4 :: (a,b,c,d) -> c
thd4 (a,b,c,d) = c

-- | Outputs the fourth element in a 4-tuple.
frt4 :: (a,b,c,d) -> d
frt4 (a,b,c,d) = d

-- | Converts a normal Played into an array containing the Played's src and dst coordinates.
playedToMove :: Played -> [(Int,Int)]
playedToMove (Played (src,dst)) = [src,dst]

-- | Converts a PlacedPawn Played into an array containing the PlacedPawn's dst coordinate.
placedPawnToMove :: Played -> [(Int,Int)]
placedPawnToMove (PlacedPawn (src,dst)) = [dst]

{- | 
    Takes in a move and a player and a GameState and returns if the move is valid in the form of
    a Played and an int representing a penalty amount. Validates move by checking source cell,
    destination cell, player type of both cells, and through verifying the destination cell through
    verifyPieceDest. If the move is invalid, returns a (Goofed,n) tuple where n is the penalty amount.
-}
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
                                         
{- |
    Given a PieceType, source Cell type, destination Cell type, and source and destination coordinates, returns
    true if the destination cell is valid, and false otherwise.
-}
verifyPieceDest   :: PieceType -> Cell -> Cell -> (Int, Int) -> (Int, Int) -> Bool
verifyPieceDest Knight _ dstCell (x1,y1) (x2,y2) = let columnDiff = abs (x2 - x1)
                                                       rowDiff = abs (y2 - y1)
                                                   in (((columnDiff==1) && (rowDiff==2)) || ((columnDiff==2) && (rowDiff==1)))                                   
verifyPieceDest Pawn WP dstCell (x1,y1) (x2,y2) = if y2==(y1+1)
                                                  then if x2==x1
                                                       then dstCell==E
                                                       else (((abs (x2-x1))==1) && ((dstCell==BP) || (dstCell==BK)))
                                                  else False
verifyPieceDest Pawn BP dstCell (x1,y1) (x2,y2) = if y2==(y1-1)
                                                  then if x2==x1
                                                       then dstCell==E
                                                       else (((abs (x2-x1))==1) && ((dstCell==WP) || (dstCell==WK)))
                                                  else False  
                                                  
{- takes in the two moves of the two players in the form of two played types, and then the current game state
and then returns an array of the modified board.
-}

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

{- takes in the played moves from both players and the board modification array and the old game state and returns 
the modified board with the penalties calculated.
-}

modifyGameState :: ((Played, Int), (Played, Int), [BoardModification]) -> GameState -> GameState
modifyGameState ((wPlay, wPenalty), (bPlay, bPenalty), mods) g = 
  GameState (bPlay)
         ((blackPen g) + bPenalty)
         (wPlay)
         ((whitePen g) + wPenalty)
         (applyBoardModifications mods (theBoard g))
         
-- | Types of BoardModifications that can be applied to the game board.
data BoardModification = Move (Int, Int) (Int, Int) -- ^ Move, represented by a destination and source cell
                       | Delete (Int, Int)          -- ^ Delete, represented by a cell to be deleted
                       | Place Cell (Int, Int)      -- ^ Place, represented by a Cell and the location for it's placement
                       
-- | apply board modifications takes in an array of board modifications and then the current board and then returns the updated board

applyBoardModifications   :: [BoardModification] -> Board -> Board
applyBoardModifications [] b = b
applyBoardModifications (x:xs) b = applyBoardModifications xs (modifyBoard x b)

-- | Is the actual act of instantianting the specific changes to the board by taking in the board and the board modifications

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

--AI Utilty function---------------------------------------------------------------

noValidMoves :: (Fractional n, Eq n, Ord n) => Player -> [(Int,n,GameState,Played)] -> Bool
noValidMoves player outcomes = isIdentical 0 (map (\(_,_,g,_) -> length $ validMoves player g) outcomes)

{- Takes in the player and the game state as well as an array of played moves and calls the apropriate functions to obtain a list
of all valid moves
-}
validMoves :: Player -> GameState -> [Played]
validMoves p g = map fst (filter validMove (map (\x -> verifyMoveLegality x p g) (possibleMoves p g)))

-- | determines if any particular pair of possibble moves is vaild

validMove :: (Played,Int) -> Bool
validMove (Played _,_) = True
validMove (_,_) = False

validPlacements :: GameState -> (Int,Int) -> [Played]
validPlacements g src = map (\dst -> PlacedPawn (src,dst)) (emptyCells g)

-- | Creates an array of all possible moves but does not account for collisions.

possibleMoves :: Player -> GameState -> [[(Int,Int)]]
possibleMoves p g = foldr (++) [] (map moves (playerCells p g))

-- | This creates every possible moves without regard for peice type

moves :: (Int,Int) -> [[(Int,Int)]]
moves src = foldr (++) [] (map (\x -> map (\y -> [src,(x,y)]) [0..4]) [0..4])

-- | creates an array of all cells

cells :: [(Int,Int)]
cells = foldr (++) [] (map (\x -> map (\y -> (x,y)) [0..4]) [0..4])

-- | Determins which cells are occupied by each player

playerCells :: Player -> GameState -> [(Int,Int)]
playerCells p g = filter (playerCell p g) cells

-- | checks if a specific cell belongs to a player

playerCell :: Player -> GameState -> (Int,Int) -> Bool
playerCell p g coord = let cell = getFromBoard (theBoard g) coord
                       in (cell/=E && (playerOf (pieceOf cell))==p)

-- | Gets the empty cells so that the pawn placement can happen in a random location
emptyCells :: GameState -> [(Int,Int)]
emptyCells g = filter (\x -> (getFromBoard (theBoard g) x)==E) cells                       

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
