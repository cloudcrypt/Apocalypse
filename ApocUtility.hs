
module ApocUtility (
    PieceType(Pawn,Knight),
    pieceTypeOf,
    samePlayer,
    reachedLastRank
    ) where

import ApocTools

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

reachedLastRank :: Played -> Player -> Bool
reachedLastRank (Played (_,(x2,y2))) White = y2==4
reachedLastRank (Played (_,(x2,y2))) Black = y2==0     