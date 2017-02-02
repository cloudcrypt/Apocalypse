
module ApocUtility (
    strategies,
    displayStrategies,
    PieceType(Pawn,Knight),
    pieceTypeOf,
    samePlayer,
    upgradeableMove,
    pieceCount
    ) where

import ApocTools
import ApocStrategyHuman

strategies  :: [(String,Chooser)]
strategies = [("human",whiteHuman)]

displayStrategies :: IO ()
displayStrategies = do 
    putStrLn "Possible strategies:"
    printStrategies strategies

printStrategies :: [(String,Chooser)] -> IO ()
printStrategies [] = return ()
printStrategies (x:xs) = do
    putStrLn ("  " ++ (fst x))
    printStrategies xs

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

upgradeableMove :: Played -> Player -> GameState -> Bool
upgradeableMove (Played (_,(x2,y2))) White g = ((getFromBoard (theBoard g) (x2,y2))==WP && y2==4)
upgradeableMove (Played (_,(x2,y2))) Black g = ((getFromBoard (theBoard g) (x2,y2))==BP && y2==0)

pieceCount     :: Board -> Player -> PieceType -> Int
pieceCount [] _ _ = 0
pieceCount (x:xs) p pt = (pieceCountRow x p pt) + (pieceCount xs p pt)

pieceCountRow  :: [Cell] -> Player -> PieceType -> Int
pieceCountRow [] _ _ = 0
pieceCountRow (E:xs) p pt = pieceCountRow xs p pt
pieceCountRow (x:xs) p pt = if ((playerOf (pieceOf x))==p && (pieceTypeOf x)==pt)
                            then 1 + (pieceCountRow xs p pt)
                            else pieceCountRow xs p pt