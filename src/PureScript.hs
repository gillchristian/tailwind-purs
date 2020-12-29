module PureScript where

import qualified Data.Bifunctor as BiF
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

data AST
  = TailwindClass String
  | CharNode
  deriving (Eq, Ord, Show)

node :: Parser AST
node = P.try className <|> charNode

className :: Parser AST
className = TailwindClass <$> (P.string "T." *> P.many P.alphaNum)

charNode :: Parser AST
charNode = CharNode <$ P.anyChar

getClassName :: AST -> Maybe String
getClassName (TailwindClass name) = Just name
getClassName _ = Nothing

tailwindClassNames :: FilePath -> Text -> Either String [String]
tailwindClassNames path contents = BiF.first show $ P.parse parser path contents
  where
    parser = Maybe.mapMaybe getClassName <$> P.many node
