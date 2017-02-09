{- |
Module: ApocInput.hs
Description: Cpsc449 W2017 - Group 24 
Copyright: (c) Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License: None (Assignment)
Portability: ghc 7.10.3 , Needs Cabal
-}
module ApocInput (
    promptLine,
    validateInputMove
    ) where

import ApocUtility   

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
