{- |
Module      : ApocStrategies
Description : Functions for getting, displaying, and parsing/validating strategies.
Copyright   : (c) 2017 Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License     : None
Stability   : experimental
Portability : ghc 7.10.2 - 8.0.2, requires System.Random
-}
module ApocStrategies (
    strategies,
    displayStrategies,
    validateStrategies,
    getStrategies
    ) where
    
import Data.Maybe (fromJust, isNothing)
import Data.Char (toUpper)
import ApocTools
import ApocStrategyHuman
import ApocStrategyGreedy
import ApocStrategyPassiveAggressive
import ApocStrategyRandom
import ApocStrategyJoke

-- | List of strategy names and their associated Chooser functions.
strategies  :: [(String,Chooser)]
strategies = [("human",human),("greedy",greedy),("passiveAggressive",passAgg),("random",randomChoice),("joke",joke)]

-- | Displays all strategies console.
displayStrategies :: IO ()
displayStrategies = do 
    putStrLn "Possible strategies:"
    printStrategies strategies

-- | Prints name of each strategy to the console, using correct formatting.
printStrategies :: [(String,Chooser)] -> IO ()
printStrategies [] = return ()
printStrategies (x:xs) = do
    putStrLn ("  " ++ (fst x))
    printStrategies xs

{- |
    Check to see if two given strategies are valid. If both strategies are valid, it will return the string,chooser pair
    for each, else it will return nothing.
-}
validateStrategies  :: [String] -> Maybe ((String,Chooser),(String,Chooser))
validateStrategies [a,b] = let s1 = validateStrategy strategies a
                               s2 = validateStrategy strategies b
                           in case (not (isNothing s1),not (isNothing s2)) of
                                (True, True) -> Just (fromJust s1,fromJust s2)
                                _ -> Nothing

{- 
    Check to see if a given strategy is valid by comparing it to an array of strategy pairs. If it is valid, it will return the 
    strategy strategy pair, else recursively validate.
-}
validateStrategy    :: [(String,Chooser)] -> String -> Maybe (String,Chooser)
validateStrategy [] s = Nothing
validateStrategy (x:xs) s = if (fst x)==s
                            then Just x
                            else validateStrategy xs s

-- | Prompts the user for the strategies of Black and White
getStrategies       :: IO [String]
getStrategies = do
    s1 <- getStrategy Black
    s2 <- getStrategy White
    return [s1,s2]    

-- |Prompt user for strategy
getStrategy         :: Player -> IO String
getStrategy p = do
    putStrLn ("Enter the strategy for " ++ (map toUpper (show p)) ++ ":")
    str <- getLine
    putStrLn str
    return str
