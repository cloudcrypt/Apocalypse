
module ApocInput (
    validateStrategies,
    getStrategies
    --promptLine,
    --validateInputMove
    ) where

import Data.Maybe (fromJust, isNothing)
import Data.Char (toUpper)
import ApocTools
import ApocUtility

validateStrategies  :: [String] -> Maybe (Chooser,Chooser)
validateStrategies [a,b] = let s1 = validateStrategy strategies a
                               s2 = validateStrategy strategies b
                           in case (not (isNothing s1),not (isNothing s2)) of
                                (True, True) -> Just (fromJust s1,fromJust s2)
                                _ -> Nothing

validateStrategy    :: [(String,Chooser)] -> String -> Maybe Chooser
validateStrategy [] s = Nothing
validateStrategy (x:xs) s = if (fst x)==s
                            then Just (snd x)
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

-- promptLine :: String -> IO String
-- promptLine prompt = do
--    putStr prompt
--    getLine 

-- validateInputMove :: String -> Either String (Maybe [(Int,Int)])
-- validateInputMove s = 
--    let list = seperate s []
--        len = length list
--        valid = ranger list
--    in case (len,valid) of
--       (4,True) -> Right (Just [((list !! 0),(list !! 1)),((list !! 2),(list !! 3))])
--       (0,True) -> Right Nothing
--       (4,False) -> Left "Integers out of Range"
--       _ -> Left ((show len) ++ " number of integers found, 4 required")             