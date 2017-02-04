
module ApocStrategyGreedy(
    greedy
    ) where

import ApocTools
import ApocUtility

greedy   :: Chooser
greedy g Normal p = return (Just (getGreedyChoice (bestMove p g)))
greedy g PawnPlacement p = return Nothing

getGreedyChoice :: Played -> [(Int,Int)]
getGreedyChoice (Played (src,dst)) = [src,dst]

bestMove :: Player -> GameState -> Played
bestMove p g = snd (foldr maxWinFactor (-10,Passed) (possibleWinFactors p g))

maxWinFactor :: (Fractional n, Eq n, Ord n) => (n,Played) -> (n,Played) -> (n,Played)
maxWinFactor (w1,p1) (w2,p2) = case (w1==w2,w1>w2) of
                                (True,_) -> (w1,p1)
                                (False,True) -> (w1,p1)
                                (_,_) -> (w2,p2)

possibleWinFactors :: (Fractional n) => Player -> GameState -> [(n,Played)]
possibleWinFactors p g = map (possibleWinFactor p g) (validMoves p g)

possibleWinFactor :: (Fractional n) => Player -> GameState -> Played -> (n,Played)
possibleWinFactor White g played = (winFactor White (modifyGameState ((played,0),(Passed,0),(addModifications played Passed g)) g),played)
possibleWinFactor Black g played = (winFactor Black (modifyGameState ((Passed,0),(played,0),(addModifications played Passed g)) g),played)

validMoves :: Player -> GameState -> [Played]
validMoves p g = map fst (filter validMove (map (\x -> verifyMoveLegality x p g) (possibleMoves p g)))

validMove :: (Played,Int) -> Bool
validMove (Played _,_) = True
validMove (_,_) = False

possibleMoves :: Player -> GameState -> [[(Int,Int)]]
possibleMoves p g = foldr (++) [] (map moves (playerCells p g))

moves :: (Int,Int) -> [[(Int,Int)]]
moves src = foldr (++) [] (map (\x -> map (\y -> [src,(x,y)]) [0..4]) [0..4])

cells :: [(Int,Int)]
cells = foldr (++) [] (map (\x -> map (\y -> (x,y)) [0..4]) [0..4])

playerCells :: Player -> GameState -> [(Int,Int)]
playerCells p g = filter (playerCell p g) cells

playerCell :: Player -> GameState -> (Int,Int) -> Bool
playerCell p g coord = let cell = getFromBoard (theBoard g) coord
                       in (cell/=E && (playerOf (pieceOf cell))==p)

winFactor :: (Fractional n) => Player -> GameState -> n
winFactor p g = let b = theBoard g
                    e = otherPlayer p
                in (fromIntegral (pieceCount b p Pawn)) - (fromIntegral (pieceCount b e Pawn)) + (0.5*(fromIntegral (pieceCount b p Knight))) - (0.4*(fromIntegral(pieceCount b e Knight)))