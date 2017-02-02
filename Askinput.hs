module GetInput where

import System.Environment
import System.IO
import Data.List
import Data.Char
import Data.Array

promptLine :: String -> IO String
promptLine prompt = do
   putStr prompt
   getLine

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

getInput :: IO (Either String (Maybe [(Int,Int)])) 
getInput = do
   line <- promptLine "Enter the move coordinates for player White in the form 'srcX srcY destX destY' [0>= n >=4, or just enter return for a 'pass'"
   return $ validateInputMove line