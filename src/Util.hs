module Util where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

startsWith :: (a -> Bool) -> [a] -> Bool
startsWith _ [] = False
startsWith f (x : _) = f x
