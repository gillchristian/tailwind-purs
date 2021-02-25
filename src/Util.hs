module Util where

import Control.Applicative (Applicative (liftA2))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

startsWith :: (a -> Bool) -> [a] -> Bool
startsWith _ [] = False
startsWith f (x : _) = f x

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
