{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman (
   human,
   whiteHuman,
   whiteHuman2,
   blackHuman,
   blackHuman2
   ) where

import ApocTools

import System.Environment
import System.IO
import Data.List
import Data.Char
import Data.Array

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
-- whiteHuman    :: Chooser
-- whiteHuman b Normal        c = return (Just [(0,0),(2,1)])
-- whiteHuman b PawnPlacement c = return (Just [(2,2)])

-- whiteHuman2    :: Chooser
-- whiteHuman2 b Normal        c = return (Just [(2,1),(1,3)])
-- whiteHuman2 b PawnPlacement c = return (Just [(2,2)])

-- blackHuman    :: Chooser
-- blackHuman b Normal        c = return (Just [(1,4),(1,3)])
-- blackHuman b PawnPlacement c = return (Just [(2,2)])

-- blackHuman2    :: Chooser
-- blackHuman2 b Normal        c = return (Just [(1,3),(1,2)])
-- blackHuman2 b PawnPlacement c = return (Just [(2,2)])
-- strategies  :: [(String,Chooser)]
-- strategies = [("human",whiteHuman)]

human   :: Chooser
human g Normal p = do
  input <- promptLine ("Enter the move coordinates for player "
                      ++ (show p)
                      ++ " in the form 'srcX srcY destX destY'\n[0 >= n >= 4, or just enter return for a 'pass'] Xn:\n")
  case validateInputMove input of
    Left s -> do
      putStrLn s
      human g Normal p
    Right maybeCoords -> return maybeCoords

human g PawnPlacement p = return Nothing


whiteHuman    :: Chooser
whiteHuman b Normal        c = return Nothing
whiteHuman b PawnPlacement c = return (Just [(2,2)])

whiteHuman2    :: Chooser
whiteHuman2 b Normal        c = return Nothing
whiteHuman2 b PawnPlacement c = return (Just [(2,2)])

blackHuman    :: Chooser
blackHuman b Normal        c = return (Just [(1,2),(1,1)])
blackHuman b PawnPlacement c = return (Just [(2,2)])

blackHuman2    :: Chooser
blackHuman2 b Normal        c = return (Just [(1,1),(1,0)])
blackHuman2 b PawnPlacement c = return (Just [(2,2)])

promptLine :: String -> IO String
promptLine prompt = do
   putStr prompt
   getLine 

validateInputMove :: String -> Either String (Maybe [(Int,Int)])
validateInputMove s = 
   let list = seperate s []
       len = length list
       valid = ranger list
   in case (len,valid) of
      (4,True) -> Right (Just [((list !! 0),(list !! 1)),((list !! 2),(list !! 3))])
      (0,True) -> Right Nothing
      (4,False) -> Left "Integers out of Range"
      _ -> Left ((show len) ++ " number of integers found, 4 required") 

seperate :: [Char] -> [Int] -> [Int]
seperate [] y = reverse y
seperate (x:xs) y = 
    if isDigit x == True
       then seperate xs ((digitToInt(x)):y)
       else
          seperate xs y

ranger :: [Int] -> Bool
ranger [] = True
ranger (x:xs) = 
    if x < 5 && x >= 0
       then ranger xs
       else False  
