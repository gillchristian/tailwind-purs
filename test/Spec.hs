{-# LANGUAGE OverloadedStrings #-}

import qualified CSS
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Lib
import qualified PureScript as PS
import Test.Hspec
import qualified Text.Parsec as P
import Text.Render

(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(===) = shouldBe

toText :: [String] -> Text
toText = T.pack . intercalate "\n"

main :: IO ()
main = hspec $ do
  describe "escapeCssName" $
    it "removes '\\'" $ do
      escapeCssName "foo\\:bar" === "foo:bar"
      escapeCssName "lg\\:hover\\:translate-x-0\\.5" === "lg:hover:translate-x-0.5"

  describe "cssToPursName" $ do
    it "camelcases" $ do
      cssToPursName "foo-bar-baz" === "fooBarBaz"
      cssToPursName "bottom:hover" === "bottomHover"
      cssToPursName "xl:w-9/12" === "xlW9d12"

    it "prefixes with '_' when starting numbers" $ do
      cssToPursName "32xl" === "_32xl"
      cssToPursName "55-random" === "_55Random"
      cssToPursName "55random" === "_55random"

    it "replaces '/' with 'd'" $ do
      cssToPursName "bottom-1/2" === "bottom1d2"
      cssToPursName "xl:w-9/12" === "xlW9d12"

    it "replaces '.' with 'p'" $ do
      cssToPursName "bottom-0.5" === "bottom0p5"
      cssToPursName "xl:w-3.5" === "xlW3p5"

    it "replaces prefix '-' with 'neg'" $ do
      cssToPursName "-32xl" === "neg32xl"
      cssToPursName "-bottom-0.5" === "negBottom0p5"

    it "replaces ':-' with 'Neg'" $ do
      cssToPursName "focus:-rotate-12" === "focusNegRotate12"
      cssToPursName "lg:-bottom-0.5" === "lgNegBottom0p5"

  describe "PureScript.tailwindClassNames" $
    it "extracts Tailwind classes from PureScript code" $ do
      let input =
            toText
              [ "module MyModule where",
                "",
                "import Data.Maybe (Maybe(..))",
                "import Tailwind as T",
                "",
                "data Action = Initialize | LoadUser",
                "",
                "type State = { users :: RemoteData String User }",
                "",
                "render { user } = ",
                "  HH.div",
                "    [ HP.classes [ T.minHScreen, T.bgGray200 ] ]",
                "    [ HH.div [ HP.classes [ T.container ] ] [ HH.text user.name ]",
                "    , HH.p [ HP.classes [ T.flex, T.flexCol ] ] [ HH.text user.name ]",
                "    ]"
              ]
          actual = PS.tailwindClassNames "PureScript.tailwindClassNames" input
          expected = Right ["minHScreen", "bgGray200", "container", "flex", "flexCol"]
      actual === expected

  describe "cssToAvailableClasses" $
    it "transforms a css ast to available classes format" $ do
      let input =
            CSS.AST
              [ -- other nodes
                CSS.Comment " some comment ",
                CSS.RuleGroup (CSS.GenericSelector "body" :| []) "margin: 0;",
                CSS.RuleGroup (CSS.GenericSelector "*" :| []) "--tw-shadow: 0 0 #0000;",
                CSS.Comment " other comment ",
                -- duplicates
                CSS.RuleGroup (CSS.ClassSelector "flex" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "flex" Nothing :| []) "display: flex;",
                -- classes with special characters
                CSS.RuleGroup (CSS.ClassSelector "camel-case-names" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "focus\\:-rotate-12" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "bottom-0\\.5" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "bottom-1/2" Nothing :| []) "display: flex;",
                CSS.RuleGroup (CSS.ClassSelector "pointer" (Just ":hover") :| []) "cursor: pointer;",
                -- multiple classes
                CSS.RuleGroup
                  ( CSS.ClassSelector "class-1" (Just ":hover")
                      :| [CSS.ClassSelector "class-2" Nothing, CSS.ClassSelector "class-3" Nothing]
                  )
                  "cursor: pointer;",
                -- media queries
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.RuleGroup
                      (CSS.ClassSelector "media-1" (Just ":hover") :| [])
                      "cursor: pointer;"
                  ],
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.MediaQuery
                      "(min-width: 320px)"
                      [CSS.RuleGroup (CSS.ClassSelector "media-2" (Just ":hover") :| []) "cursor: pointer;"]
                  ],
                -- other queries
                CSS.Query
                  "keyframes"
                  "spin"
                  [CSS.RuleGroup (CSS.GenericSelector "to" :| []) "transform: rotate(360deg);"]
              ]
          expected =
            intercalate
              "\n"
              [ "bottom0p5;bottom-0.5",
                "bottom1d2;bottom-1/2",
                "camelCaseNames;camel-case-names",
                "class1;class-1",
                "class2;class-2",
                "class3;class-3",
                "flex;flex",
                "focusNegRotate12;focus:-rotate-12",
                "lgHoverTranslateY1;lg:hover:translate-y-1",
                "media1;media-1",
                "media2;media-2",
                "pointer;pointer"
              ]
      cssToAvailableClasses input === expected

  describe "cssFile" $
    it "parses a CSS file" $ do
      let input =
            toText
              [ "/* a comment */",
                "body {",
                "  margin: 0;",
                "}",
                "",
                "b,",
                "strong {",
                "  font-weight: bolder;",
                "}",
                "",
                "/* another",
                "   comment */",
                "",
                "@keyframes spin {",
                "  to {",
                "    transform: rotate(360deg);",
                "  }",
                "}",
                "",
                "@media (min-width: 640px) {",
                "  .sm\\:container {",
                "    width: 100%;",
                "  }",
                "}",
                ".-mx-3\\.5 {",
                "  margin-left: -0.875rem;",
                "  margin-right: -0.875rem;",
                "}",
                "",
                ".mt-0 {",
                "  margin-top: 0px;",
                "}",
                "@media (min-width: 1536px) {",
                "  @media (min-width: 640px) {",
                "    .\\32xl\\:container {",
                "      display: flex;",
                "    }",
                "  }",
                "",
                "  .\\32xl\\:space-y-0 > :not([hidden]) ~ :not([hidden]) {",
                "    display: flex;",
                "  }",
                "}"
              ]
          expected =
            CSS.AST
              [ CSS.Comment " a comment ",
                CSS.RuleGroup (CSS.GenericSelector "body" :| []) "\n  margin: 0;\n",
                CSS.RuleGroup (CSS.GenericSelector "b" :| [CSS.GenericSelector "strong"]) "\n  font-weight: bolder;\n",
                CSS.Comment " another\n   comment ",
                CSS.Query
                  "keyframes"
                  "spin"
                  [CSS.RuleGroup (CSS.GenericSelector "to" :| []) "\n    transform: rotate(360deg);\n  "],
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.RuleGroup
                      (CSS.ClassSelector "sm\\:container" Nothing :| [])
                      "\n    width: 100%;\n  "
                  ],
                CSS.RuleGroup (CSS.ClassSelector "-mx-3\\.5" Nothing :| []) "\n  margin-left: -0.875rem;\n  margin-right: -0.875rem;\n",
                CSS.RuleGroup (CSS.ClassSelector "mt-0" Nothing :| []) "\n  margin-top: 0px;\n",
                CSS.MediaQuery
                  "(min-width: 1536px)"
                  [ CSS.MediaQuery "(min-width: 640px)" [CSS.RuleGroup (CSS.ClassSelector "\\32xl\\:container" Nothing :| []) "\n      display: flex;\n    "],
                    CSS.RuleGroup (CSS.ClassSelector "\\32xl\\:space-y-0" (Just " > :not([hidden]) ~ :not([hidden])") :| []) "\n    display: flex;\n  "
                  ]
              ]
      P.parse CSS.cssFile "cssFile" input === Right expected

  describe "CSS.ruleGroup & render @CSSNode" $
    it "node == (parse CSS.ruleGroup . render) node" $ do
      let body = "\n  display: flex;  margin-bottom: 10px;\n"
          selectors =
            [ CSS.ClassSelector "some-class" Nothing :| [],
              CSS.ClassSelector "some-class" (Just ":hover") :| [],
              CSS.ClassSelector "some-class" (Just ":focus") :| [CSS.ClassSelector "another-class" Nothing],
              CSS.ClassSelector "bottom-1/2" Nothing :| [],
              CSS.ClassSelector "focus\\:-rotate-12" Nothing :| [],
              CSS.ClassSelector "bottom-0\\.5" Nothing :| [],
              CSS.ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| [],
              CSS.ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| [CSS.ClassSelector "lg\\:hover\\:translate-y-1" Nothing]
            ]
          groups = fmap (flip CSS.RuleGroup body) selectors
          actual = fmap (P.parse CSS.ruleGroup "ruleGroup_render" . T.pack . render) groups
          expected = fmap Right groups
      mapM_ (uncurry shouldBe) $ zip actual expected
