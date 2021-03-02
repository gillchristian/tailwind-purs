module AvailableClasses where

import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Text.Render

data ClassName = ClassName
  { classPursName :: String,
    classCssName :: String
  }
  deriving (Eq, Show, Ord)

instance Render ClassName where
  render (ClassName name css) =
    unlines
      [ "-- | " <> name,
        name <> " :: ClassName",
        name <> " = ClassName \"" <> css <> "\""
      ]

cssClass :: Parser ClassName
cssClass =
  ClassName
    <$> P.manyTill (P.noneOf ";") (P.char ';')
    <*> P.many1 (P.noneOf "\n")

availableClasses :: Parser [ClassName]
availableClasses = cssClass `P.endBy` P.spaces <* P.eof
