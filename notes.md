Parse, print and save (produces same output as input)

```haskell
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

TIO.writeFile "output.html"
  =<< TL.toStrict
  <$> renderTokens
  <$> parseTokens
  <$> TIO.readFile "example.html"
```

Comment case

```haskell
fromPredicate (Text.null . Text.strip) comment'
where
  comment' =
    Text.unlines $
      fmap (indent 1 "--" <>) $
        dropWhileEnd (Text.null . Text.strip) $
          dropWhile (Text.null . Text.strip) $
            Text.lines $
              toStrict $
                toLazyText comment
```

Statefull version

```
{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad (forM_)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Foldable (Foldable (fold))
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Tree (Forest, Tree (Node))
import Text.HTML.Parser
import Text.HTML.Tree

type Doc = Forest Token

type Node = Tree Token

-- printDoc :: Doc -> Text
-- printDoc nodes = fold $ intersperse "\n" $ fmap printTree nodes

-- printTree :: Node -> Text
-- printTree (Node node rest) =
--   printToken node
--     <> printDoc rest

indent :: Int -> Text -> Text
indent n content = Text.replicate n "  " <> content

printToken :: Token -> State PrintState Text
printToken (Comment _) = pure "" -- TODO handle comments
printToken (Doctype _) = pure ""
printToken (TagOpen name []) = do
  n <- State.gets stLevel
  isFirstChild <- State.gets stIsFirstChild
  -- isFirstNode <- State.gets stIsFirstNode

  State.modify $ \s -> s {stLevel = n + 1, stIsFirstNode = False}

  -- case (isFirstNode, isFirstChild) of
  --   (True, _) -> pure $ "HH." <> name <> "\n" <> indent n "[]\n" <> indent n "[ "

  if isFirstChild
    then pure $ " HH." <> name <> "\n" <> indent n "[]\n" <> indent n "[ "
    else pure $ indent n ", HH." <> name <> "\n" <> indent n "[]\n" <> indent n "[ "
printToken (TagOpen name attrs) = do
  n <- State.gets stLevel
  State.modify $ \s -> s {stLevel = n + 1, stIsFirstNode = False}
  pure $ content n
  where
    content n =
      indent n "HH." <> name <> "\n"
        <> (indent (n + 1) "[ " <> fold (intersperse ", " (fmap printAttr attrs)) <> " ]\n")
        <> indent (n + 1) "[ "
printToken (TagClose _) = do
  n <- State.gets stLevel
  State.modify $ \s -> s {stLevel = n - 1, stIsFirstNode = False}
  pure $ indent (n + 1) "]\n"
printToken (TagSelfClose name attrs) = do
  n <- State.gets stLevel
  State.modify $ \s -> s {stLevel = n - 1, stIsFirstNode = False}
  pure $ content n
  where
    content n =
      indent n "HH." <> name <> "\n"
        <> (indent n "[ " <> fold (intersperse ", " (fmap printAttr attrs)) <> " ]\n")
printToken (ContentText content) | Text.null $ Text.strip content = pure ""
printToken (ContentText content) = pure $ "HH.text \"" <> Text.strip content <> "\"\n"
printToken (ContentChar content) = pure $ "HH.text \"" <> Text.singleton content <> "\"\n"

printAttr :: Attr -> Text
printAttr (Attr name value) = "HP." <> name <> " " <> value

data PrintState = PrintState
  { stLevel :: Int,
    stIsFirstNode :: Bool,
    stIsFirstChild :: Bool
  }

printTokens :: [Token] -> State PrintState Text
printTokens [] = pure ""
printTokens (token : rest) = do
  a <- printToken token
  b <- printTokens rest
  pure $ a <> b

test :: IO ()
test = do
  tokens <- canonicalizeTokens . parseTokens <$> TIO.readFile "example.html"
  forM_ tokens print
  putStrLn "\n-------\n"
  TIO.putStrLn $
    State.evalState (printTokens tokens) $
      PrintState {stLevel = 0, stIsFirstChild = True, stIsFirstNode = True}

-- case tokensToForest tokens of
--   Right doc -> TIO.putStrLn $ printDoc doc
--   Left err -> print err

-- [ TagOpen "div" [Attr "class" "min-h-screen bg-gray-100"]
-- , ContentText "\n  "
-- , TagOpen "span" []
-- , ContentText "foo"
-- , TagClose "span"
-- , ContentText "\n  hola\n"
-- , TagClose "div"
-- , ContentText "\n"
-- ]

-- foo =
--   [ Node
--       { rootLabel = TagOpen "div" [Attr "class" "min-h-screen bg-gray-100"],
--         subForest = [Node {rootLabel = ContentText "\n  hola\n", subForest = []}]
--       },
--     Node
--       { rootLabel = ContentText "\n\n",
--         subForest = []
--       }
--   ]
--
-- [ Node
--     { rootLabel = TagOpen "div" [Attr "class" "min-h-screen bg-gray-100"]
--     , subForest = [ Node {rootLabel = ContentText "\n  ", subForest = []}
--                   , Node {rootLabel = TagOpen "span" [], subForest = [Node {rootLabel = ContentText "foo", subForest = []}]}
--                   , Node {rootLabel = ContentText "\n  hola\n", subForest = []}
--                   ]
--     }
-- , Node {rootLabel = ContentText "\n", subForest = []}]
```

