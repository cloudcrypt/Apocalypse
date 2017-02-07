
module ApocStrategyGreedy(
    greedy
    ) where

--import System.Random
import ApocTools
import ApocUtility

greedy   :: Chooser
greedy g Normal p = do
    let m = bestMove p g
    return (Just (getGreedyChoice m))
greedy g PawnPlacement p = return Nothing

getGreedyChoice :: Played -> [(Int,Int)]
getGreedyChoice (Played (src,dst)) = [src,dst]

optimalMove :: Player -> GameState -> Played
optimalMove p g = frt4 $ foldr minMoves (1000,-1000,g,Passed) (map (neededMoves p) (possibleOutcomes p g 0))

neededMoves :: (Fractional n, Eq n, Ord n) => Player -> (Int,n,GameState,Played) -> (Int,n,GameState,Played)
neededMoves player (moves,winF,g,played) = case ((length (validMoves player g))>0 && (pieceCount (theBoard g) (otherPlayer player) Pawn)>0) of
                                            False -> (moves,winF,g,played)
                                            True -> neededMoves player (moves+1,winF,(applyBestMove player g),played)

applyBestMove :: Player -> GameState -> GameState
applyBestMove White g = let p = bestMove White g
                        in modifyGameState ((p,0),(Passed,0),(addModifications p Passed g)) g
applyBestMove Black g = let p = bestMove Black g
                        in modifyGameState ((Passed,0),(p,0),(addModifications p Passed g)) g                        

bestMove :: Player -> GameState -> Played
bestMove p g = frt4 $ foldr maxWinFactor (10000,-1000,g,Passed) (expandOutcomes p (possibleOutcomes p g 0))

expandOutcomes :: (Fractional n, Eq n, Ord n) => Player -> [(Int,n,GameState,Played)] -> [(Int,n,GameState,Played)]
expandOutcomes player outcomes = case ((length outcomes)>1 && (isIdentical (snd4 (outcomes !! 0)) (map snd4 outcomes))) of 
                                    False -> outcomes
                                    True -> expandOutcomes player $ foldr (++) [] (map (expandOutcome player) outcomes)

expandOutcome :: (Fractional n, Eq n, Ord n) => Player -> (Int,n,GameState,Played) -> [(Int,n,GameState,Played)]
expandOutcome player (_,n,g,played) = map (\(newMoves,newWinFactor,newG,_) -> (newMoves,newWinFactor,newG,played)) (possibleOutcomes player g n)

minMoves :: (Fractional n, Eq n, Ord n) => (Int,n,GameState,Played) -> (Int,n,GameState,Played) -> (Int,n,GameState,Played)
minMoves (m1,w1,g1,p1) (m2,w2,g2,p2) = case (m1==m2,m1<m2) of
                                        (True,_) -> (m1,w1,g1,p1)
                                        (False,True) -> (m1,w1,g1,p1)
                                        (_,_) -> (m2,w2,g2,p2)                        

maxWinFactor :: (Fractional n, Eq n, Ord n) => (Int,n,GameState,Played) -> (Int,n,GameState,Played) -> (Int,n,GameState,Played)
maxWinFactor (m1,w1,g1,p1) (m2,w2,g2,p2) = case (w1==w2,w1>w2) of
                                            (True,_) -> (m1,w1,g1,p1)
                                            (False,True) -> (m1,w1,g1,p1)
                                            (_,_) -> (m2,w2,g2,p2)

possibleOutcomes :: (Fractional n) => Player -> GameState -> n -> [(Int,n,GameState,Played)]
possibleOutcomes p g n = map (possibleOutcome p g n) (validMoves p g)

possibleOutcome :: (Fractional n) => Player -> GameState -> n -> Played -> (Int,n,GameState,Played)
possibleOutcome White g n played = let newState = modifyGameState ((played,0),(Passed,0),(addModifications played Passed g)) g
                                   in (1,n+(winFactor White newState),newState,played)
possibleOutcome Black g n played = let newState = modifyGameState ((Passed,0),(played,0),(addModifications played Passed g)) g
                                   in (1,n+(winFactor Black newState),newState,played)

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