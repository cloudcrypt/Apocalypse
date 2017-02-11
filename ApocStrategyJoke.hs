--ApocJoke.hs
{- |

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
