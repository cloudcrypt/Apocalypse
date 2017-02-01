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

ranger :: Int -> [Int] -> Bool
ranger 0 [] = True
ranger 4 [] = True
ranger 3 [] = False
ranger 2 [] = False
ranger 1 [] = False
ranger k (x:xs) = 
    if x < 5 && x >= 0
       then ranger (k-1) xs
       else False


pMenu :: IO String
pMenu = do
   putStr "1 Aggression Strategy\n"
   putStr "2 Passive Strategy\n"
   putStr "3 Daniel's Super Smart Strategy\n"
   putStr "4 human strategy\n\n"
   getLine


main = do
   choice <- pMenu
   line1 <- promptLine "Enter the move coordinate for player White in the form 'srcX srcY destX destY 0 <= n < 5, or just enter return for a pass W2: "
           if ranger  4 ((seperate line1) []) == False
             then main
             else putStr "Valid input.\n"