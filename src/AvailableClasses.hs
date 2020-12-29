module AvailableClasses where

import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Text.Render

data CssClass = CssClass
  { classPursName :: String,
    classCssName :: String
  }
  deriving (Eq, Show, Ord)

instance Render CssClass where
  render (CssClass name css) =
    unlines
      [ "-- | " <> name,
        name <> " :: ClassName",
        name <> " = ClassName \"" <> css <> "\""
      ]

cssClass :: Parser CssClass
cssClass =
  CssClass
    <$> P.manyTill (P.noneOf ";") (P.char ';')
    <*> P.many1 (P.noneOf "\n")

availableClasses :: Parser [CssClass]
availableClasses = cssClass `P.endBy` P.spaces <* P.eof
