{-# LANGUAGE OverloadedStrings #-}

module Html where

import Prelude hiding (unlines)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Forest, Tree (Node))
import Text.HTML.Parser
import Text.HTML.Tree
import PureScript
import Data.Maybe (mapMaybe)
import Data.Bifunctor (Bifunctor(bimap))

indent :: Int -> Text -> Text
indent n content = Text.replicate (n * 2) " " <> content

type Doc = Forest Token

type Node = Tree Token

printDoc :: Doc -> Text
printDoc = unlines . mapMaybe (printTree True 0)

printChildren :: Int -> [Node] -> Text
printChildren n =
  Text.intercalate ("\n" <> indent (n + 1) ", ") . mapMaybe (printTree False (n + 1))

tagName :: Text -> Text
tagName name = "HH." <> name

emptyBrackets :: Int -> Bool -> Text
emptyBrackets n True = indent (n + 1) "[]"
emptyBrackets n False = indent (n + 2) "[]"

openBracket :: Int -> Bool -> Text
openBracket n True = indent (n + 1) "[ "
openBracket n False = indent (n + 2) "[ "

closeBracket :: Int -> Bool -> Text
closeBracket n True = indent (n + 1) "]"
closeBracket n False = indent (n + 2) "]"

unlines :: [Text] -> Text
unlines = Text.intercalate "\n"

-- TODO: pretty printing is a mess right now
printTree :: Bool -> Int -> Node -> Maybe Text
printTree _ _ (Node (Comment _) _) = Nothing
printTree _ _ (Node (Doctype _) _) = Nothing
printTree _ _ (Node (ContentText content) _) = textNode content
printTree _ _ (Node (ContentChar content) _) = Just $ "HH.text \"" <> Text.singleton content <> "\""
printTree _ _ (Node (TagClose _) _) = Nothing
printTree _ _ (Node (TagOpen name []) []) = Just $ "HH." <> name <> " [] []"
printTree isTopLevel n (Node (TagOpen name []) [Node (ContentText content) _]) =
  Just $ unlines
    [ tagName name
    , emptyBrackets n isTopLevel
    , case textNode content of
        Just txt -> openBracket n isTopLevel <> txt <> " ]"
        Nothing -> emptyBrackets n isTopLevel
    ]
printTree isTopLevel n (Node (TagOpen name []) rest) =
  Just $ unlines
    [ tagName name
    , emptyBrackets n isTopLevel
    , openBracket n isTopLevel <> printChildren n rest
    , closeBracket n isTopLevel
    ]
printTree isTopLevel n (Node (TagOpen name attrs) []) =
  Just $ unlines
    [ tagName name
    , openBracket n isTopLevel <> Text.intercalate ", " (mapMaybe printAttr attrs) <> " ]"
    , emptyBrackets n isTopLevel
    ]
printTree isTopLevel n (Node (TagOpen name attrs) [Node (ContentText content) _]) =
  Just $ unlines
    [ tagName name
    , openBracket n isTopLevel <> Text.intercalate ", " (mapMaybe printAttr attrs) <> " ]"
    , case textNode content of
        Just txt -> openBracket n isTopLevel <> txt <> " ]"
        Nothing -> emptyBrackets n isTopLevel
    ]
printTree isTopLevel n (Node (TagOpen name attrs) rest) =
  Just $ unlines
    [ tagName name
    , openBracket n isTopLevel <> Text.intercalate ", " (mapMaybe printAttr attrs) <> " ]"
    , openBracket n isTopLevel <> printChildren n rest
    , closeBracket n isTopLevel
    ]
printTree _ _ (Node (TagSelfClose name []) _) = Just $ "HH." <> name <> " []"
printTree isTopLevel n (Node (TagSelfClose name attrs) _) =
  Just $ unlines
    [ tagName name
    , openBracket n isTopLevel <> Text.intercalate ", " (mapMaybe printAttr attrs) <> " ]"
    ]

textNode :: Text -> Maybe Text
textNode content | Text.null $ Text.strip content = Nothing
textNode content = Just $ "HH.text \"" <> Text.strip content <> "\""

mkClassName :: Text -> Text
mkClassName = ("T." <>) . Text.pack . cssToPursName . Text.unpack

printAttr :: Attr -> Maybe Text
printAttr (Attr "class" value) = Just $ "HP.classes [ " <> cs <> " ]"
  where
  cs = Text.intercalate ", " $ mkClassName <$> Text.words value
printAttr (Attr "type" "button") = Just "HP.type_ HP.ButtonButton"
printAttr (Attr "type" "reset") = Just "HP.type_ HP.ButtonRest"
printAttr (Attr "type" "submit") = Just "HP.type_ HP.ButtonSubmit"
printAttr (Attr "type" _) = Nothing -- TODO
printAttr (Attr name _) | "data-" `Text.isPrefixOf` name = Nothing
printAttr (Attr name _) | "aria-" `Text.isPrefixOf` name = Nothing
printAttr (Attr name value) = Just $ "HP." <> name <> " \"" <> value <> "\""

htmlToHalogen :: Text -> Either Text Text
htmlToHalogen =
  bimap (const "Something went wrong. Probably an <input> ins't self closed as it should") printDoc
    . tokensToForest
    . canonicalizeTokens
    . parseTokens
