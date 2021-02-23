{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Tree (Forest, Tree (Node))
import PureScript
import Text.HTML.Parser
import Text.HTML.Tree
import Prelude hiding (unlines)

indent :: Int -> Text -> Text
indent n content = Text.replicate (n * 2) " " <> content

type Doc = Forest Token

printDoc :: Doc -> Text
printDoc = unlines . mapMaybe printTree

printChildren :: Doc -> Text
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

fromPredicate :: (a -> Bool) -> a -> Maybe a
fromPredicate pred a
  | pred a = Just a
  | otherwise = Nothing

printTree :: Tree Token -> Maybe Text
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
isIgnoredAttr (Attr name _) | "data-" `Text.isPrefixOf` name = True
isIgnoredAttr (Attr name _) | "aria-" `Text.isPrefixOf` name = True
isIgnoredAttr _ = False

textNode :: Text -> Maybe Text
textNode content | Text.null $ Text.strip content = Nothing
textNode content = Just $ "HH.text \"" <> Text.strip content <> "\""

mkClassName :: Text -> Text
mkClassName = ("T." <>) . Text.pack . cssToPursName . Text.unpack

printAttr :: Attr -> Maybe Text
-- type button
printAttr (Attr "type" "button") = Just "HP.type_ HP.ButtonButton"
printAttr (Attr "type" "reset") = Just "HP.type_ HP.ButtonRest"
printAttr (Attr "type" "submit") = Just "HP.type_ HP.ButtonSubmit"
-- type input
printAttr (Attr "type" "text") = Just "HP.type_ HP.InptuText"
printAttr (Attr "type" "password") = Just "HP.type_ HP.InptuPassword"
printAttr (Attr "type" "email") = Just "HP.type_ HP.InptuEmail"
printAttr (Attr "type" "number") = Just "HP.type_ HP.InptuNumber"
printAttr (Attr "type" "checkbox") = Just "HP.type_ HP.InptuCheckbox"
printAttr (Attr "type" "radio") = Just "HP.type_ HP.InptuRadio"
printAttr (Attr "type" "file") = Just "HP.type_ HP.InptuFile"
printAttr (Attr "type" "color") = Just "HP.type_ HP.InputColor"
printAttr (Attr "type" "date") = Just "HP.type_ HP.InputDate"
printAttr (Attr "type" "hidden") = Just "HP.type_ HP.InputHidden"
printAttr (Attr "type" "image") = Just "HP.type_ HP.InputImage"
printAttr (Attr "type" "month") = Just "HP.type_ HP.InputMonth"
printAttr (Attr "type" "range") = Just "HP.type_ HP.InputRange"
printAttr (Attr "type" "search") = Just "HP.type_ HP.InputSearch"
printAttr (Attr "type" "tel") = Just "HP.type_ HP.InputTel"
printAttr (Attr "type" "time") = Just "HP.type_ HP.InputTime"
printAttr (Attr "type" "Url") = Just "HP.type_ HP.InputUrl"
printAttr (Attr "type" "week") = Just "HP.type_ HP.InputWeek"
-- others
printAttr (Attr "class" value) = Just $ "HP.classes [ " <> classList' <> " ]"
  where
    cs = Text.words value
    classList = Text.intercalate ", " $ mkClassName <$> cs
    classList' =
      if length cs > 5 || Text.length classList > 80
        then Text.intercalate ("\n" <> indent 1 ", ") $ mkClassName <$> Text.words value
        else classList
printAttr (Attr "role" value) = Just $ "HPA.role \"" <> value <> "\""
printAttr (Attr "id" value) = Just $ "HP.id_ \"" <> value <> "\""
printAttr (Attr "autocomplete" "") = Just "HP.autocomplete false"
printAttr (Attr "autocomplete" _) = Just "HP.autocomplete true"
printAttr (Attr "method" _) = Nothing
printAttr (Attr "action" _) = Nothing
printAttr (Attr name value) = Just $ "HP." <> name <> " \"" <> value <> "\""

errorMsg :: Text
errorMsg =
  "Something went wrong. Probably an input or img isnt' self closed (eg. <input />)"

htmlToHalogen :: Text -> Either Text Text
htmlToHalogen =
  bimap (const errorMsg) printDoc
    . tokensToForest
    . canonicalizeTokens
    . parseTokens
