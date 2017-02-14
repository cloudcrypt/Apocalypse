{- |
Module      : ApocStrategyMinimax
Description : Implementation of an AI using a minimax algorithm for the Apocalypse game.
Copyright   : (c) 2017 Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License     : None
Stability   : experimental
Portability : ghc 7.10.2 - 8.0.2, requires Data.List, Data.Ord, Data.Tree
-}

module ApocStrategyMinimax(
    minimax
    ) where

import Data.Maybe (fromJust, isNothing)
import ApocTools
import AIUtility
import Data.List
import Data.Ord
import Data.Tree
-- import Data.Tree.Pretty

{- |
    Given a gameState, Normal or PawnPlacement move, and a Player, creates a tree of all possible
    gamestates resulting from the current gamestate. All leaf nodes are evaluated
    (using a simple heuristic) and the tree is folded to output the optimal move.
-}
minimax   :: Chooser
minimax g Normal p = return (Just (getMove p g))
minimax g PawnPlacement p = return (Just [((getMovePawn p g) !! 1)])

-- |Returns the optimal move resulting from folding the tree for the player passed in.
getMove Black g = getMoveFromPlayed $ blackPlay (snd (((levels (makeTreeToDepth 0 (0,g))) !! 1) !! (findMove Black g) ))
getMove White g = getMoveFromPlayed $ whitePlay (snd (((levels (makeTreeToDepth 0 (0,g))) !! 1) !! (findMove White g) ))
-- | Finds the index of the best move from all possible next moves.
findMove p g = indexOfBestMove (foldRose $ evaluateLeaves p 1 (makeTreeToDepth 1 (1,g)))

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{- |
    This set of functions, given a depth and a gameState,
    generates a tree of all possible moves resulting from
    that gamestate to the given depth.
-}

-- | Main function for generating trees given only a gamestate and no depth.
makeTree :: (Int,GameState) -> Tree (Int,GameState)
makeTree g = Node g (makeSubTrees (getSubForest (Node g [])))

-- | Main function for generating the tree for a given gamestate to a given depth.
makeTreeToDepth :: Int -> (Int,GameState) -> Tree (Int,GameState)
makeTreeToDepth d g = Node g (makeSubTrees' d (getSubForest (Node g [])))

-- | Returns the generated subForest of the tree passed in.
getSubForest :: Tree (Int,GameState) -> [Tree (Int,GameState)]
getSubForest t = subForest $ makeChildren t

-- | Takes a subForest and makes new SubTrees for each.
makeSubTrees :: [Tree (Int,GameState)] -> [Tree (Int,GameState)]
makeSubTrees f = map (\x -> Node (rootLabel x) (makeSubTrees (getSubForest x))) f

-- | Takes a subForest and makes new SubTrees for each, terminates early when it hits the max depth.
makeSubTrees' :: Int -> [Tree (Int,GameState)] -> [Tree (Int,GameState)]
makeSubTrees' 0 f = f
makeSubTrees' d f = map (\x -> Node (rootLabel x) (makeSubTrees' (d-1) (getSubForest x))) f

-- | Takes a tree and returns it with its subForest generated according to a generation function.
makeChildren :: Tree (Int,GameState) -> Tree (Int,GameState)
makeChildren t = Node (rootLabel t) (map (\x -> forestHelper x) (newGameStates (rootLabel t)))

-- | Helper function for makeChildren.
forestHelper :: (Int,GameState) -> Tree (Int,GameState)
forestHelper g = Node g []

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    The set of functions responsible for generating branching gamestates/evaluated gamestates.
-}

-- | Given a GameState, this function returns a list of all possible GameStates that could result from it.
newGameStates :: (Int,GameState) -> [(Int,GameState)]
newGameStates g = map (\x -> toTuple ((fst g)-1) x) (concat $ map (\y -> (map (\x -> performMoves (Just x) (Just y) (snd g)) (map (\x -> getMoveFromPlayed x) (validMoves White (snd g))))) (map (\x -> getMoveFromPlayed x) (validMoves Black (snd g))))

-- | Helper function for newGameStates - Puts an int and a gamestate into a 2-tuple.
toTuple :: Int -> GameState -> (Int,GameState)
toTuple d g = (d,g)

-- | Takes a player, a depth and a tree of 2-tuples, returns an evaluated version of the tree.
evaluateLeaves :: Player -> Int -> Tree (Int,GameState) -> Tree String
evaluateLeaves p d t = fmap (\x -> if ((fst x)==(d)) then "Current" else if ((fst x)==(-1) || gameOverCheck (snd x) || validMoves White (snd x)==[] || validMoves Black (snd x)==[]) then show $ evaluate p (snd x) else show 100) t

-- | Takes a tree of 2-tuples (depth, gamestate), and returns a tree with just the gameboards.
showBoards :: Tree (Int,GameState) -> Tree String
showBoards t = fmap (\x -> show $ theBoard (snd x)) t

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    The set of functions responsible for calculating values.
-}

-- | Evaluation function.
evaluate :: Player -> GameState -> Int
evaluate p g = let b = theBoard g
                   e = otherPlayer p
               in (500*((pieceCount b p Pawn) - (pieceCount b e Pawn))) + (((pieceCount b p Knight) - (pieceCount b e Knight))) + 10*((numberOfAttacks p g) - (numberOfAttacks e g))

-- | Takes a player and a gamestate, and returns the number of attacks it can make in that state.
numberOfAttacks :: Player -> GameState -> Int
numberOfAttacks p g = sum $ map (\x -> countAttacks p g x) (map (\y -> getDestination y) (validMoves p g))

-- | Takes a player and a gametstate, and counts the number of attacks it can make in that state.
countAttacks :: Player -> GameState -> (Int,Int) -> Int
countAttacks p g coord
  | (getFromBoard (theBoard g) coord)/=E = if (playerOf (pieceOf (getFromBoard (theBoard g) coord))/=p) then 1 else 0
  | otherwise = 0
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    The set of functions responsible for folding the tree.
-}

-- | Folds the tree up to where all possible next moves are listed from the starting gamestate.
foldRose :: Tree String -> Tree String
foldRose t = fmapTree' (\x -> if (rootLabel x /= "Current") && ((hasChildren x) && (not (foldr (||) False (map hasChildren (subForest x))))) then updateNode x else x) t

-- | Custom map method for trees - performs the map function on the actual tree rather than the value at each node.
fmapTree' f (Node x ts) = f (Node x (map (fmapTree' f) ts))

-- | Updates the node to its value if it is a successor to only one set of subtrees.
updateNode n = Node {rootLabel = show $ average (getValues n), subForest = []}

-- | Finds the index of the best move from all possible next moves.
indexOfBestMove t = maxIndex $ map (read::String->Double) ((levels t) !! 1)

-- | Helper function for indexOfBestMove.
-- | Function found at http://stackoverflow.com/questions/9270478/efficiently-find-indices-of-maxima-of-a-list
maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

-- | Takes a list of numbers as strings, and returns their average.
average :: [String] -> Double
average list = (sum (map (\x -> read x)list) / fromIntegral (length list))

-- | Gets all the values from the subforest of a tree.
getValues t = map rootLabel (subForest t)

-- | Checks whether a the node of a given tree has children.
hasChildren :: Tree String -> Bool
hasChildren (Node x []) = False
hasChildren (Node x y) = True

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    The set of functions responsible for generating all possible moves,
    and returning coordinates for normal plays.
-}

-- | Returns the set of source and destination coordinates given an instance of Played.
getMoveFromPlayed :: Played -> [(Int,Int)]
getMoveFromPlayed (Played (src,dst)) = [src,dst]

-- | Returns the destination coordinates given an instance of Played.
getDestination :: Played -> (Int,Int)
getDestination (Played (src,dst)) = dst

-- | Returns a list of all valid moves given a player and a gamestate.
validMoves :: Player -> GameState -> [Played]
validMoves p g = map fst (filter (\y -> validMove g y) (map (\x -> verifyMoveLegality x p g) (possibleMoves p g)))

-- | Checks if a move is valid for the given gamestate - returns a boolean value.
validMove :: GameState -> (Played,Int) -> Bool
validMove g (Played _,_)
  | gameOverCheck g == False = True
  | otherwise = False
validMove g (_,_) = False

-- | Returns a list of all possible moves given a player and a gamestate.
possibleMoves :: Player -> GameState -> [[(Int,Int)]]
possibleMoves p g = foldr (++) [] (map moves (playerCells p g))

-- | Returns all possible moves from source coordinates.
moves :: (Int,Int) -> [[(Int,Int)]]
moves src = foldr (++) [] (map (\x -> map (\y -> [src,(x,y)]) [0..4]) [0..4])

-- | Returns a list of coordinates of every cell on the board.
cells :: [(Int,Int)]
cells = foldr (++) [] (map (\x -> map (\y -> (x,y)) [0..4]) [0..4])

-- | Return a list of coordinates of all cells containing the given player's pieces.
playerCells :: Player -> GameState -> [(Int,Int)]
playerCells p g = filter (playerCell p g) cells

-- | Checks every cell to see whether it is occupied by a given player's piece.
playerCell :: Player -> GameState -> (Int,Int) -> Bool
playerCell p g coord = let cell = getFromBoard (theBoard g) coord
                       in (cell/=E && (playerOf (pieceOf cell))==p)

-- | Checks to see whether the passed gamestate is an endstate.
gameOverCheck :: GameState -> Bool
gameOverCheck g
   | wPawns==0 && bPawns==0 = True
   | wPawns==0 = True
   | bPawns==0 = True
   | otherwise = False
   where wPawns = pieceCount (theBoard g) White Pawn
         bPawns = pieceCount (theBoard g) Black Pawn

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    Pawn placement specific functions.
    The set of functions responsible for generating all possible moves,
    and returning coordinates specifically for pawn placement plays.
    Also includes generating trees specific for pawn placement plays.
    These functions are slight variations of the functions for normal moves.
-}

getMovePawn Black g = getMoveFromPlacedPawn $ blackPlay (snd (((levels (makeTreeToDepthPawn 0 (0,g))) !! 1) !! (findMovePawn Black g) ))
getMovePawn White g = getMoveFromPlacedPawn $ whitePlay (snd (((levels (makeTreeToDepthPawn 0 (0,g))) !! 1) !! (findMovePawn White g) ))

findMovePawn p g = indexOfBestMove (foldRose $ evaluateLeaves p 0 (makeTreeToDepthPawn 0 (0,g)))

makeTreeToDepthPawn :: Int -> (Int,GameState) -> Tree (Int,GameState)
makeTreeToDepthPawn d g = Node g (makeSubTrees' d (getSubForestPawn (Node g [])))

getSubForestPawn :: Tree (Int,GameState) -> [Tree (Int,GameState)]
getSubForestPawn t = subForest $ makeChildrenPawn t

makeChildrenPawn :: Tree (Int,GameState) -> Tree (Int,GameState)
makeChildrenPawn t = Node (rootLabel t) (map (\x -> forestHelper x) (newGameStatesPawn (rootLabel t)))

newGameStatesPawn :: (Int,GameState) -> [(Int,GameState)]
newGameStatesPawn g = map (\x -> toTuple ((fst g)-1) x) $ map (\y -> performPawnPlace (Just [(y !! 1)]) (y !! 0) Black (snd g)) (map (\x -> getMoveFromPlacedPawn x) (validPawnMoves Black (snd g)))

possiblePawnMoves :: Player -> GameState -> [[(Int,Int)]]
possiblePawnMoves p g = foldr (++) [] (map moves (pawnCell p g))

promotionCells :: Player -> [(Int,Int)]
promotionCells White = [(0,4), (1,4), (2,4), (3,4), (4,4)]
promotionCells Black = [(0,0), (1,0), (2,0), (3,0), (4,0)]

pawnCell :: Player -> GameState -> [(Int,Int)]
pawnCell p g = filter (\y -> (y /= (0,0))) (map (\x -> if ((getFromBoard (theBoard g) x)/=E && (playerOf (pieceOf (getFromBoard (theBoard g) x)))==p && (pieceTypeOf ((getFromBoard (theBoard g) x)))==Pawn) then x else (0,0)) (promotionCells p))

pawnCellGet :: [(Int,Int)] -> (Int,Int)
pawnCellGet [(x,y)] = (x,y)

getMoveFromPlacedPawn :: Played -> [(Int,Int)]
getMoveFromPlacedPawn (PlacedPawn (src,dst)) = [src,dst]

validPawnMoves :: Player -> GameState -> [Played]
validPawnMoves p g = map fst (filter validPawnMove (map (\x -> verifyPawnPlaceLegality x g) (possiblePawnMoves p g)))

validPawnMove :: (Played,Int) -> Bool
validPawnMove (PlacedPawn _,_) = True
validPawnMove (_,_) = False

performPawnPlace  :: Maybe [(Int,Int)] -> (Int,Int) -> Player -> GameState -> GameState
performPawnPlace move src p g = modifyGameState (verifyPawnPlace move src p g) g

------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    The set of functions responsible for verifying moves,
    and 'performing' moves for normal plays.
-}

-- | Returns an updated gamestate given a normal move.
performMoves    :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState -> GameState
performMoves white black g = modifyGameState (verifyMoves white black g) g

-- | Verifies the validity of a normal move given source and destination coordinates from both players, as well as a gamestate.
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

------------------------------------------------------------------------------------------------------------------------------------------------------
{- |
    Pawn placement specific functions.
    The set of functions responsible for verifying moves, and 'performing' moves for normal plays.
-}

{- |
    Verifies a pawn placement move, generating a tuple that can be passed to
    modifyGameState. To generate the tuple, checks move, assigning NullPlacedPawn and penalty
    of 1 if Nothing, or a (Played,Int) tuple from verifyPawnPlaceLegality otherwise, and generates
    a list of board modifications based on the verified placement move.
-}
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

{- |
    Verifies a the legality of a pawn placement move, from src cell xy1
    to dst cell xy2. If move is valid, outputs a Played of PlacedPawn and penalty
    of 0. If move is invalid, outputs a Played of BadPlacedPawn and a penalty of 1.
-}
verifyPawnPlaceLegality :: [(Int, Int)] -> GameState -> (Played, Int)
verifyPawnPlaceLegality (xy1:xy2:_) g = case (getFromBoard (theBoard g) xy2) of
                                    E -> ((PlacedPawn (xy1,xy2)), 0)
                                    _ -> ((BadPlacedPawn (xy1,xy2)), 1)
