{- |
Module: ApocUtility.hs
Description: Cpsc449 W2017 - Group 24 
Copyright: (c) Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License: None (Assignment)
Portability: ghc 7.10.3 , Needs Cabal
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
    verifyMoveLegality,
    addModifications,
    BoardModification(Move,Delete,Place),
    modifyGameState,
    validMoves,
    cells
    ) where

import ApocTools
import Data.Char (isDigit, digitToInt)

data PieceType  = Pawn | Knight deriving (Eq)
-- | Given a 'Cell', return the corresponding 'Piece'.
pieceTypeOf     :: Cell -> PieceType
pieceTypeOf BK  = Knight
pieceTypeOf WK  = Knight
pieceTypeOf BP  = Pawn
pieceTypeOf WP  = Pawn 

samePlayer              :: Cell -> Cell -> Bool
samePlayer cell1 cell2  = case ((cell1==E) || (cell2==E)) of
                          True -> False
                          False -> (playerOf (pieceOf cell1))==(playerOf (pieceOf cell2))

otherPlayer     :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White                  

-- | Deals with the upgrading of pawns pattern matched, for each player

upgradeableMove :: Played -> Player -> GameState -> Bool
upgradeableMove (Played (_,(x2,y2))) White g = ((getFromBoard (theBoard g) (x2,y2))==WP && y2==4)
upgradeableMove (Played (_,(x2,y2))) Black g = ((getFromBoard (theBoard g) (x2,y2))==BP && y2==0)

-- | calculates the number of pawns for each player

pieceCount     :: Board -> Player -> PieceType -> Int
pieceCount [] _ _ = 0
pieceCount (x:xs) p pt = (pieceCountRow x p pt) + (pieceCount xs p pt)

-- | calculates the number of pieces in each row and returns that as an int

pieceCountRow  :: [Cell] -> Player -> PieceType -> Int
pieceCountRow [] _ _ = 0
pieceCountRow (E:xs) p pt = pieceCountRow xs p pt
pieceCountRow (x:xs) p pt = if ((playerOf (pieceOf x))==p && (pieceTypeOf x)==pt)
                            then 1 + (pieceCountRow xs p pt)
                            else pieceCountRow xs p pt 

-- | Determins if an item is an element within an array

isIdentical :: Eq a => a -> [a] -> Bool
isIdentical a [] = True
isIdentical a (x:xs) = case a==x of
                        True -> isIdentical a xs
                        False -> False                   

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

thd4 :: (a,b,c,d) -> c
thd4 (a,b,c,d) = c

frt4 :: (a,b,c,d) -> d
frt4 (a,b,c,d) = d

playedToMove :: Played -> [(Int,Int)]
playedToMove (Played (src,dst)) = [src,dst]

{- | Takes in a move and a player and a game state and returns if the move is valid in the form of of a played and a int 
 representing a invalid move and penalty 
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
                                         
{- Takes in the piece type and two cells representing the destination of the piece as well as the source
coordinates and based on the piece type will make the piece move to the new location.
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
         
{-data type board modification is a list of one of three types, a move, which represents the destination and source 
delete which takes the coordinates of the location to delete and place cell which contains the coordinates of a placement
-}

data BoardModification = Move (Int, Int) (Int, Int)
                       | Delete (Int, Int)
                       | Place Cell (Int, Int)
                       
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


{- Takes in the player and the game state as well as an array of played moves and calls the apropriate functions to obtain a list
of all valid moves
-}
validMoves :: Player -> GameState -> [Played]
validMoves p g = map fst (filter validMove (map (\x -> verifyMoveLegality x p g) (possibleMoves p g)))

-- | determines if any particular pair of possibble moves is vaild

validMove :: (Played,Int) -> Bool
validMove (Played _,_) = True
validMove (_,_) = False

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
