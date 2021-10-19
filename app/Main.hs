module Main where

import Lib
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 run
