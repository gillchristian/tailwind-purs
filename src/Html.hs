{-# LANGUAGE OverloadedStrings #-}

module Html where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function ((&))
import Data.List (dropWhileEnd)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Tree (Forest, Tree (Node))
import PureScript
import Text.Casing (pascal)
import qualified Text.HTML.Parser as HTML
import qualified Text.HTML.Tree as HTML
import Prelude hiding (unlines)

newtype AttrName = AttrName Text
  deriving (Eq, Ord, Show)

newtype AttrValue = AttrValue Text
  deriving (Eq, Ord, Show)

data Attr = Attr AttrName AttrValue
  deriving (Eq, Ord, Show)

newtype HtmlElem = HtmlElem Text
  deriving (Eq, Ord, Show)

type Comment = Maybe Text

data HtmlAst
  = HtmlNode HtmlElem Comment [Attr] [HtmlAst]
  | HtmlLeaf HtmlElem Comment [Attr]
  | HtmlText Comment Text
  | HtmlEmpty Comment
  deriving (Eq, Ord, Show)

indent :: Int -> Text -> Text
indent n content = Text.replicate (n * 2) " " <> content

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

printComment :: Text -> Maybe Text
printComment comment =
  fromPredicate (not . Text.null . Text.strip) comment'
  where
    comment' =
      Text.lines comment
        & dropWhile (Text.null . Text.strip)
        & dropWhileEnd (Text.null . Text.strip)
        & fmap (indent 1 "-- " <>)
        & Text.unlines

elemName :: HtmlElem -> Text
elemName (HtmlElem name) = "HH." <> name

mbUnline :: [Maybe Text] -> Maybe Text
mbUnline = Just . unlines . catMaybes

printTree :: HtmlAst -> Maybe Text
printTree (HtmlEmpty comment) = comment >>= printComment
printTree (HtmlText comment txt) | Text.null $ Text.strip txt = comment >>= printComment
printTree (HtmlText comment txt) =
  mbUnline
    [ comment >>= printComment,
      -- TODO: handle multiline text
      Just $ "HH.text \"" <> Text.strip txt <> "\""
    ]
printTree (HtmlLeaf elem comment []) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem <> emptyBrackets
    ]
printTree (HtmlLeaf elem comment attrs) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem,
      Just $ printAttrList elem attrs
    ]
printTree (HtmlNode elem comment [] []) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem <> emptyBrackets <> emptyBrackets
    ]
printTree (HtmlNode elem comment attrs []) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem,
      Just $ printAttrList elem attrs,
      Just emptyBrackets
    ]
printTree (HtmlNode elem comment [] [child@(HtmlText _ _)]) =
  mbUnline
    [ comment >>= printComment,
      Just $ case printTree child of
        Just childTxt ->
          elemName elem <> emptyBrackets <> openBracket <> childTxt <> closeBracket
        Nothing -> elemName elem <> emptyBrackets <> emptyBrackets
    ]
printTree (HtmlNode elem comment [] children) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem,
      Just emptyBrackets,
      Just $ openBracket <> printChildren children,
      Just closeBracket
    ]
printTree (HtmlNode elem comment attrs [child@(HtmlText _ _)]) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem,
      Just $ printAttrList elem attrs,
      Just $ maybe emptyBrackets (\childTxt -> openBracket <> childTxt <> closeBracket) (printTree child)
    ]
printTree (HtmlNode elem comment attrs children) =
  mbUnline
    [ comment >>= printComment,
      Just $ elemName elem,
      Just $ printAttrList elem attrs,
      Just $ openBracket <> printChildren children,
      Just closeBracket
    ]

printChildren :: [HtmlAst] -> Text
printChildren =
  Text.intercalate ("\n" <> indent 1 ", ") . mapMaybe printTree

mkClassName :: Text -> Text
mkClassName = ("T." <>) . Text.pack . cssToPursName . Text.unpack

printAttr :: HtmlElem -> Attr -> Maybe Text
printAttr (HtmlElem "button") (Attr (AttrName "type") (AttrValue type_)) =
  Just $ ("HP.type_ HP.Button" <>) . Text.pack . pascal . Text.unpack $ type_
printAttr (HtmlElem "input") (Attr (AttrName "type") (AttrValue type_)) =
  Just $ ("HP.type_ HP.Input" <>) . Text.pack . pascal . Text.unpack $ type_
printAttr _ (Attr (AttrName "class") (AttrValue value)) = Just $ "HP.classes [ " <> classList' <> " ]"
  where
    cs = Text.words value
    classList = Text.intercalate ", " $ mkClassName <$> cs
    classList' =
      if length cs > 5 || Text.length classList > 80
        then Text.intercalate ("\n" <> indent 1 ", ") $ mkClassName <$> Text.words value
        else classList
printAttr _ (Attr (AttrName "role") (AttrValue value)) = Just $ "HPA.role \"" <> value <> "\""
printAttr _ (Attr (AttrName "id") (AttrValue value)) = Just $ "HP.id_ \"" <> value <> "\""
printAttr _ (Attr (AttrName "autocomplete") (AttrValue "")) = Just "HP.autocomplete false"
printAttr _ (Attr (AttrName "autocomplete") _) = Just "HP.autocomplete true"
printAttr _ (Attr (AttrName "method") _) = Nothing
printAttr _ (Attr (AttrName "action") _) = Nothing
printAttr _ (Attr (AttrName name) (AttrValue value)) = Just $ "HP." <> name <> " \"" <> value <> "\""

isIgnoredAttr :: Attr -> Bool
isIgnoredAttr (Attr (AttrName name) _) | "data-" `Text.isPrefixOf` name = True
isIgnoredAttr (Attr (AttrName name) _) | "aria-" `Text.isPrefixOf` name = True
isIgnoredAttr _ = False

printAttrList :: HtmlElem -> [Attr] -> Text
printAttrList elem attrs =
  openBracket <> Text.intercalate divider (mapMaybe (printAttr elem) attrs') <> closeBracket
  where
    attrs' = filter (not . isIgnoredAttr) attrs
    divider = if length attrs' > 2 then "\n" <> indent 1 ", " else ", "

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

errorMsg :: Text
errorMsg =
  "Something went wrong. Probably an input or img isnt' self closed (eg. <input />)"

printDoc :: Forest HTML.Token -> Text
printDoc = unlines . mapMaybe printTree . mapMaybe (handleEmpty . toHtmlAst)

htmlToHalogen :: Text -> Either Text Text
htmlToHalogen =
  bimap (const errorMsg) printDoc
    . HTML.tokensToForest
    . HTML.canonicalizeTokens
    . HTML.parseTokens
