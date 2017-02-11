{- |
Module: ApocStrategies.hs
Description: Cpsc449 W2017 - Group 24 
Copyright: (c) Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License: None (Assignment)
Portability: ghc 7.10.3 , Needs Cabal
-}
module ApocStrategies(
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

strategies  :: [(String,Chooser)]
strategies = [("human",human),("greedy",greedy),("passiveAggressive",passAgg),("random",randomChoice),("joke",joke)]

displayStrategies :: IO ()
displayStrategies = do 
    putStrLn "Possible strategies:"
    printStrategies strategies

printStrategies :: [(String,Chooser)] -> IO ()
printStrategies [] = return ()
printStrategies (x:xs) = do
    putStrLn ("  " ++ (fst x))
    printStrategies xs

validateStrategies  :: [String] -> Maybe ((String,Chooser),(String,Chooser))
validateStrategies [a,b] = let s1 = validateStrategy strategies a
                               s2 = validateStrategy strategies b
                           in case (not (isNothing s1),not (isNothing s2)) of
                                (True, True) -> Just (fromJust s1,fromJust s2)
                                _ -> Nothing

validateStrategy    :: [(String,Chooser)] -> String -> Maybe (String,Chooser)
validateStrategy [] s = Nothing
validateStrategy (x:xs) s = if (fst x)==s
                            then Just x
                            else validateStrategy xs s

getStrategies       :: IO [String]
getStrategies = do
    s1 <- getStrategy Black
    s2 <- getStrategy White
    return [s1,s2]    

getStrategy         :: Player -> IO String
getStrategy p = do
    putStrLn ("Enter the strategy for " ++ (map toUpper (show p)) ++ ":")
    str <- getLine
    putStrLn str
    return str
