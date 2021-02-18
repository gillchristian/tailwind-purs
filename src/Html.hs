{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Forest, Tree (Node))
import PureScript
import Text.HTML.Parser
import Text.HTML.Tree
import Prelude hiding (unlines)

indent :: Int -> Text -> Text
indent n content = Text.replicate (n * 2) " " <> content

type Doc = Forest Token

type Node = Tree Token

printDoc :: Doc -> Text
printDoc = unlines . mapMaybe printTree

printChildren :: [Node] -> Text
printChildren =
  Text.intercalate ("\n" <> indent 1 ", ") . mapMaybe printTree

tagName :: Text -> Text
tagName name = "HH." <> name

emptyBrackets :: Text
emptyBrackets = indent 1 "[]"

openBracket :: Text
openBracket = indent 1 "[ "

closeBracket :: Text
closeBracket = indent 1 "]"

unlines :: [Text] -> Text
unlines = Text.intercalate "\n"

-- TODO: pretty printing is a mess right now
printTree :: Node -> Maybe Text
printTree (Node (Comment _) _) = Nothing
printTree (Node (Doctype _) _) = Nothing
printTree (Node (ContentText content) _) = textNode content
printTree (Node (ContentChar content) _) = Just $ "HH.text \"" <> Text.singleton content <> "\""
printTree (Node (TagClose _) _) = Nothing
printTree (Node (TagOpen name []) []) = Just $ "HH." <> name <> " [] []"
printTree (Node (TagOpen name []) [Node (ContentText content) _]) =
  Just $
    unlines
      [ tagName name,
        emptyBrackets,
        case textNode content of
          Just txt -> openBracket <> txt <> " ]"
          Nothing -> emptyBrackets
      ]
printTree (Node (TagOpen name []) rest) =
  Just $
    unlines
      [ tagName name,
        emptyBrackets,
        openBracket <> printChildren rest,
        closeBracket
      ]
printTree (Node (TagOpen name attrs) []) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        emptyBrackets
      ]
printTree (Node (TagOpen name attrs) [Node (ContentText content) _]) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        case textNode content of
          Just txt -> openBracket <> txt <> closeBracket
          Nothing -> emptyBrackets
      ]
printTree (Node (TagOpen name attrs) rest) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        openBracket <> printChildren rest,
        closeBracket
      ]
printTree (Node (TagSelfClose name []) _) = Just $ "HH." <> name <> " []"
printTree (Node (TagSelfClose name attrs) _) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs
      ]

printAttrList :: [Attr] -> Text
printAttrList attrs =
  openBracket <> Text.intercalate divider (mapMaybe printAttr attrs') <> closeBracket
  where
    attrs' = filter (not . isIgnoredAttr) attrs
    divider = if length attrs' > 2 then "\n" <> indent 1 ", " else ", "

isIgnoredAttr :: Attr -> Bool
isIgnoredAttr (Attr "type" "button") = False
isIgnoredAttr (Attr "type" "reset") = False
isIgnoredAttr (Attr "type" "submit") = False
isIgnoredAttr (Attr "type" _) = True
isIgnoredAttr (Attr name _) | "data-" `Text.isPrefixOf` name = True
isIgnoredAttr (Attr name _) | "aria-" `Text.isPrefixOf` name = True
isIgnoredAttr _ = False

textNode :: Text -> Maybe Text
textNode content | Text.null $ Text.strip content = Nothing
textNode content = Just $ "HH.text \"" <> Text.strip content <> "\""

mkClassName :: Text -> Text
mkClassName = ("T." <>) . Text.pack . cssToPursName . Text.unpack

-- TODO: handle "type" for other than button
printAttr :: Attr -> Maybe Text
printAttr (Attr "class" value) = Just $ "HP.classes [ " <> classList' <> " ]"
  where
    cs = Text.words value
    classList = Text.intercalate ", " $ mkClassName <$> cs
    classList' =
      if length cs > 5 || Text.length classList > 80
        then Text.intercalate ("\n" <> indent 1 ", ") $ mkClassName <$> Text.words value
        else classList
printAttr (Attr "type" "button") = Just "HP.type_ HP.ButtonButton"
printAttr (Attr "type" "reset") = Just "HP.type_ HP.ButtonRest"
printAttr (Attr "type" "submit") = Just "HP.type_ HP.ButtonSubmit"
printAttr (Attr name value) = Just $ "HP." <> name <> " \"" <> value <> "\""

htmlToHalogen :: Text -> Either Text Text
htmlToHalogen =
  bimap (const "Something went wrong. Probably an <input> ins't self closed as it should") printDoc
    . tokensToForest
    . canonicalizeTokens
    . parseTokens
