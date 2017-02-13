{- |
Module      : ApocJoke
Description : Joke Strategy that only passes on its turn
Copyright   : (c) 2017 Daniel Dastoor, James Gilders, Carlin Liu, Teresa Van, Thomas Vu
License     : None
Stability   : experimental
Portability : ghc 7.10.2 - 8.0.2, requires System.Random
-}


module ApocStrategyJoke (
    joke
    ) where

import ApocTools
import ApocUtility
import System.Random


joke :: Chooser
joke gs Normal p = return Nothing
joke gs PawnPlacement p = return Nothing
