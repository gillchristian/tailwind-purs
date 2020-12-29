module Text.Render where

class Render a where
  render :: a -> String
