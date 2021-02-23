{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function ((&))
import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Tree (Forest, Tree (Node))
import PureScript
import qualified Text.HTML.Parser as HTML
import qualified Text.HTML.Tree as HTML
import Prelude hiding (unlines)

indent :: Int -> Text -> Text
indent n content = Text.replicate (n * 2) " " <> content

type Doc = Forest HTML.Token

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

printTree :: Tree HTML.Token -> Maybe Text
printTree (Node (HTML.Comment comment) _) =
  fromPredicate (not . Text.null . Text.strip) comment'
  where
    comment' =
      toLazyText comment
        & toStrict
        & Text.lines
        & dropWhile (Text.null . Text.strip)
        & dropWhileEnd (Text.null . Text.strip)
        & fmap (indent 1 "--" <>)
        & Text.unlines
printTree (Node (HTML.Doctype _) _) = Nothing
printTree (Node (HTML.ContentText content) _) = textNode content
printTree (Node (HTML.ContentChar content) _) = textNode (Text.singleton content)
printTree (Node (HTML.TagClose _) _) = Nothing
printTree (Node (HTML.TagOpen "textarea" []) []) = Just "HH.textarea []"
printTree (Node (HTML.TagOpen name []) []) = Just $ "HH." <> name <> " [] []"
printTree (Node (HTML.TagOpen name []) [Node (HTML.ContentText content) _]) =
  Just $
    unlines
      [ tagName name,
        emptyBrackets,
        case textNode content of
          Just txt -> openBracket <> txt <> " ]"
          Nothing -> emptyBrackets
      ]
printTree (Node (HTML.TagOpen name []) rest) =
  Just $
    unlines
      [ tagName name,
        emptyBrackets,
        openBracket <> printChildren rest,
        closeBracket
      ]
printTree (Node (HTML.TagOpen "textarea" attrs) []) =
  Just $
    unlines
      [ tagName "textarea",
        printAttrList attrs
      ]
printTree (Node (HTML.TagOpen name attrs) []) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        emptyBrackets
      ]
printTree (Node (HTML.TagOpen name attrs) [Node (HTML.ContentText content) _]) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        case textNode content of
          Just txt -> openBracket <> txt <> closeBracket
          Nothing -> emptyBrackets
      ]
printTree (Node (HTML.TagOpen name attrs) rest) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs,
        openBracket <> printChildren rest,
        closeBracket
      ]
printTree (Node (HTML.TagSelfClose name []) _) = Just $ "HH." <> name <> " []"
printTree (Node (HTML.TagSelfClose name attrs) _) =
  Just $
    unlines
      [ tagName name,
        printAttrList attrs
      ]

printAttrList :: [HTML.Attr] -> Text
printAttrList attrs =
  openBracket <> Text.intercalate divider (mapMaybe printAttr attrs') <> closeBracket
  where
    attrs' = filter (not . isIgnoredAttr) attrs
    divider = if length attrs' > 2 then "\n" <> indent 1 ", " else ", "

isIgnoredAttr :: HTML.Attr -> Bool
isIgnoredAttr (HTML.Attr name _) | "data-" `Text.isPrefixOf` name = True
isIgnoredAttr (HTML.Attr name _) | "aria-" `Text.isPrefixOf` name = True
isIgnoredAttr _ = False

textNode :: Text -> Maybe Text
textNode content | Text.null $ Text.strip content = Nothing
textNode content = Just $ "HH.text \"" <> Text.strip content <> "\""

mkClassName :: Text -> Text
mkClassName = ("T." <>) . Text.pack . cssToPursName . Text.unpack

printAttr :: HTML.Attr -> Maybe Text
-- type button
printAttr (HTML.Attr "type" "button") = Just "HP.type_ HP.ButtonButton"
printAttr (HTML.Attr "type" "reset") = Just "HP.type_ HP.ButtonRest"
printAttr (HTML.Attr "type" "submit") = Just "HP.type_ HP.ButtonSubmit"
-- type input
printAttr (HTML.Attr "type" "text") = Just "HP.type_ HP.InptuText"
printAttr (HTML.Attr "type" "password") = Just "HP.type_ HP.InptuPassword"
printAttr (HTML.Attr "type" "email") = Just "HP.type_ HP.InptuEmail"
printAttr (HTML.Attr "type" "number") = Just "HP.type_ HP.InptuNumber"
printAttr (HTML.Attr "type" "checkbox") = Just "HP.type_ HP.InptuCheckbox"
printAttr (HTML.Attr "type" "radio") = Just "HP.type_ HP.InptuRadio"
printAttr (HTML.Attr "type" "file") = Just "HP.type_ HP.InptuFile"
printAttr (HTML.Attr "type" "color") = Just "HP.type_ HP.InputColor"
printAttr (HTML.Attr "type" "date") = Just "HP.type_ HP.InputDate"
printAttr (HTML.Attr "type" "hidden") = Just "HP.type_ HP.InputHidden"
printAttr (HTML.Attr "type" "image") = Just "HP.type_ HP.InputImage"
printAttr (HTML.Attr "type" "month") = Just "HP.type_ HP.InputMonth"
printAttr (HTML.Attr "type" "range") = Just "HP.type_ HP.InputRange"
printAttr (HTML.Attr "type" "search") = Just "HP.type_ HP.InputSearch"
printAttr (HTML.Attr "type" "tel") = Just "HP.type_ HP.InputTel"
printAttr (HTML.Attr "type" "time") = Just "HP.type_ HP.InputTime"
printAttr (HTML.Attr "type" "Url") = Just "HP.type_ HP.InputUrl"
printAttr (HTML.Attr "type" "week") = Just "HP.type_ HP.InputWeek"
-- others
printAttr (HTML.Attr "class" value) = Just $ "HP.classes [ " <> classList' <> " ]"
  where
    cs = Text.words value
    classList = Text.intercalate ", " $ mkClassName <$> cs
    classList' =
      if length cs > 5 || Text.length classList > 80
        then Text.intercalate ("\n" <> indent 1 ", ") $ mkClassName <$> Text.words value
        else classList
printAttr (HTML.Attr "role" value) = Just $ "HPA.role \"" <> value <> "\""
printAttr (HTML.Attr "id" value) = Just $ "HP.id_ \"" <> value <> "\""
printAttr (HTML.Attr "autocomplete" "") = Just "HP.autocomplete false"
printAttr (HTML.Attr "autocomplete" _) = Just "HP.autocomplete true"
printAttr (HTML.Attr "method" _) = Nothing
printAttr (HTML.Attr "action" _) = Nothing
printAttr (HTML.Attr name value) = Just $ "HP." <> name <> " \"" <> value <> "\""

errorMsg :: Text
errorMsg =
  "Something went wrong. Probably an input or img isnt' self closed (eg. <input />)"

htmlToHalogen :: Text -> Either Text Text
htmlToHalogen =
  bimap (const errorMsg) printDoc
    . HTML.tokensToForest
    . HTML.canonicalizeTokens
    . HTML.parseTokens

htmlToAst :: Text -> Either Text [HtmlAst]
htmlToAst =
  bimap (const errorMsg) (mapMaybe (handleEmpty . toHtmlAst))
    . HTML.tokensToForest
    . HTML.canonicalizeTokens
    . HTML.parseTokens

attr2attr :: HTML.Attr -> Attr
attr2attr (HTML.Attr name value) = Attr (AttrName name) (AttrValue value)

toHtmlAst :: Tree HTML.Token -> HtmlAst
toHtmlAst tree =
  case go tree of
    Nothing -> HtmlText Nothing ""
    Just (Right node) -> node
    Just (Left comment) -> HtmlText (Just comment) ""
  where
    go :: Tree HTML.Token -> Maybe (Either Text HtmlAst)
    -- Leaf
    go (Node (HTML.TagSelfClose name attrs) _) =
      Just $ Right $ HtmlLeaf (HtmlElem name) Nothing (fmap attr2attr attrs)
    go (Node (HTML.TagOpen "textarea" attrs) _) =
      Just $ Right $ HtmlLeaf (HtmlElem "textarea") Nothing (fmap attr2attr attrs)
    go (Node (HTML.TagOpen "input" attrs) _) =
      Just $ Right $ HtmlLeaf (HtmlElem "input") Nothing (fmap attr2attr attrs)
    go (Node (HTML.TagOpen "img" attrs) _) =
      Just $ Right $ HtmlLeaf (HtmlElem "img") Nothing (fmap attr2attr attrs)
    -- Node
    go (Node (HTML.TagOpen name attrs) rest) =
      mapMaybe go rest
        & handleComments
        & mapMaybe handleEmpty
        & HtmlNode (HtmlElem name) Nothing (fmap attr2attr attrs)
        & Right
        & Just
    -- Comment
    go (Node (HTML.Comment comment) _) = Just $ Left $ toStrict $ toLazyText comment
    -- Ignored
    go (Node (HTML.TagClose _) _) = Nothing
    go (Node (HTML.Doctype _) _) = Nothing
    -- Text
    go (Node (HTML.ContentText txt) _) | Text.null $ Text.strip txt = Nothing
    go (Node (HTML.ContentText txt) _) = Just $ Right $ HtmlText Nothing txt
    go (Node (HTML.ContentChar txt) _) | Text.null $ Text.strip $ Text.singleton txt = Nothing
    go (Node (HTML.ContentChar txt) _) = Just $ Right $ HtmlText Nothing $ Text.singleton txt

handleComments :: [Either Text HtmlAst] -> [HtmlAst]
handleComments [] = []
handleComments [Right node] = [node]
handleComments [Left comment] = [HtmlEmpty (Just comment)]
handleComments ((Right node) : rest) = node : handleComments rest
handleComments ((Left comment) : (Right node) : rest) = insertComment comment node : handleComments rest
handleComments ((Left commentA) : (Left commentB) : rest) = handleComments $ Left (commentA <> "\n" <> commentB) : rest

insertComment :: Text -> HtmlAst -> HtmlAst
insertComment comment (HtmlNode elm (Just prevComment) attrs rest) = HtmlNode elm (Just $ comment <> "\n" <> prevComment) attrs rest
insertComment comment (HtmlNode elm Nothing attrs rest) = HtmlNode elm (Just comment) attrs rest
insertComment comment (HtmlLeaf elm (Just prevComment) attrs) = HtmlLeaf elm (Just $ comment <> "\n" <> prevComment) attrs
insertComment comment (HtmlLeaf elm Nothing attrs) = HtmlLeaf elm (Just comment) attrs
insertComment comment (HtmlText (Just prevComment) txt) = HtmlText (Just $ comment <> "\n" <> prevComment) txt
insertComment comment (HtmlText Nothing txt) = HtmlText (Just comment) txt
insertComment comment (HtmlEmpty (Just prevComment)) = HtmlEmpty (Just $ comment <> "\n" <> prevComment)
insertComment comment (HtmlEmpty Nothing) = HtmlEmpty (Just comment)

handleEmpty :: HtmlAst -> Maybe HtmlAst
handleEmpty (HtmlText Nothing "") = Nothing
handleEmpty (HtmlText comment "") = Just $ HtmlEmpty comment
handleEmpty node = Just node

newtype AttrName = AttrName Text
  deriving (Eq, Ord, Show)

newtype AttrValue = AttrValue Text
  deriving (Eq, Ord, Show)

data Attr = Attr AttrName AttrValue
  deriving (Eq, Ord, Show)

newtype HtmlElem = HtmlElem Text
  deriving (Eq, Ord, Show)

type Comment = Maybe Text

-- HERE !!! Implement pretty print of this instead of the Tree Token
data HtmlAst
  = HtmlNode HtmlElem Comment [Attr] [HtmlAst]
  | HtmlLeaf HtmlElem Comment [Attr]
  | HtmlText Comment Text
  | HtmlEmpty Comment
  deriving (Eq, Ord, Show)
