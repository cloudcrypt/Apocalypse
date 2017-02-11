
module ApocStrategyHuman (
  human
  ) where

import Data.Maybe (fromJust, isNothing)
import Data.Foldable(find)
import Text.Read(readMaybe)
import ApocTools

human                   :: Chooser
human board Normal player =
    do move <- readNPairs 2
                          ("Enter the move coordinates for player "
                           ++ (show player)
                           ++ " in the form 'srcX srcY destX destY'\n"
                           ++ "[0 >= n >= 4, or just enter return for a 'pass'] "
                           ++ (if player==White then "W" else "B")
                           ++ "2:\n")
                          (\x -> x>=0 && x<=4)
       return move
human board PawnPlacement player =
    do move <- readNPairs 1
                          ("Enter the coordinates to place the pawn for player "
                           ++ (show player)
                           ++ " in the form 'destX destY':\n"
                           ++ "[0 >= n >= 4] "
                           ++ (if player==White then "W" else "B")
                           ++ "1:\n")
                          (\x -> x>=0 && x<=4)
       return move

list2pairs            :: [a] -> [(a,a)]
list2pairs []         = []
list2pairs (x0:x1:xs) = (x0,x1):list2pairs xs

{- | Reads a pair of pairs from standard input in the form "int1 int2 int3 int4".  If user
     just types a return, Nothing is returned, otherwise if the user types other than
     4 well-formed integers, then a subsequent attempt is done until we get correct
     input.
-}
readNPairs        :: Int -> String -> (Int->Bool)-> IO (Maybe [(Int,Int)])
readNPairs n prompt f =
    do putStrLn prompt
       line <- getLine
       putStrLn line
       let lst = words line
        in if length lst == 0
           then return Nothing
           else
             if length lst < (2*n)
             then do putStrLn $ "Bad input! (" ++ line ++ ") \nPlease enter "
                                ++ show (n*2) ++ " integers in the form '"
                                ++ (foldl (++) " " [show i++" "|i<-[1..n*2]]) ++ "'" -- count up to n
                     readNPairs n prompt f
             else case readInts (n*2) lst f of
                     Nothing   -> do putStrLn $ "Bad input: one or more integers is manformed! ("
                                              ++ line ++ ") \nPlease enter "
                                              ++ show (n*2) ++ " integers in the form '"
                                              ++ (foldl (++) " " [show i++" "|i<-[1..n*2]]) ++ "'" -- count up to n
                                     readNPairs n prompt f
                     Just x    -> return $ Just $ list2pairs x


{- | Converts a list of Strings into a list of Ints that obey constraint f.
     If one or more of the strings does not parse into an Int, or if it fails constraint
     f, then return Nothing.
-}
readInts :: Int -> [String] -> (Int->Bool) -> Maybe [Int]
readInts n xs f = let ints = take n $ map (readMaybe :: String -> Maybe Int) xs -- convert each from String to Int
                                                                                -- setting each to Nothing if failure
                   in if find (isNothing) ints == Nothing -- check all reads were successful
                      then let ints2 = (map (fromJust) ints) -- strip the "Just" from each element
                            in if find (==False) (map f ints2) == Nothing -- check constraint
                               then Just ints2 -- success
                               else Nothing    -- failure (constraint f)
                      else Nothing             -- failure (int conversion)

