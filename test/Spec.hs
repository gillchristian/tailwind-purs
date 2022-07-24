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

singletonClass :: String -> Maybe String -> CSS.Selector
singletonClass cx mod = CSS.ClassSelector $ CSS.CssClass cx mod :| []

main :: IO ()
main = hspec $ do
  describe "escapeCssName" $
    it "removes '\\'" $ do
      escapeCssName "foo\\:bar" === "foo:bar"
      escapeCssName "lg\\:hover\\:translate-x-0\\.5" === "lg:hover:translate-x-0.5"

  describe "cssToPursName" $ do
    it "camelcases" $ do
      PS.cssToPursName "foo-bar-baz" === "fooBarBaz"
      PS.cssToPursName "bottom:hover" === "bottomHover"
      PS.cssToPursName "xl:w-9/12" === "xlW9d12"

    it "prefixes with '_' when starting numbers" $ do
      PS.cssToPursName "32xl" === "_32xl"
      PS.cssToPursName "55-random" === "_55Random"
      PS.cssToPursName "55random" === "_55random"

    it "replaces '/' with 'd'" $ do
      PS.cssToPursName "bottom-1/2" === "bottom1d2"
      PS.cssToPursName "xl:w-9/12" === "xlW9d12"

    it "replaces '.' with 'p'" $ do
      PS.cssToPursName "bottom-0.5" === "bottom0p5"
      PS.cssToPursName "xl:w-3.5" === "xlW3p5"

    it "replaces prefix '-' with 'neg'" $ do
      PS.cssToPursName "-32xl" === "neg32xl"
      PS.cssToPursName "-bottom-0.5" === "negBottom0p5"

    it "replaces ':-' with 'Neg'" $ do
      PS.cssToPursName "focus:-rotate-12" === "focusNegRotate12"
      PS.cssToPursName "lg:-bottom-0.5" === "lgNegBottom0p5"

  describe "PureScript.usedTailwindClassNames" $
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
          actual = PS.usedTailwindClassNames "PureScript.usedTailwindClassNames" input
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
                CSS.RuleGroup (singletonClass "flex" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "flex" Nothing :| []) "display: flex;",
                -- classes with several classes
                CSS.RuleGroup
                  ( ( CSS.ClassSelector $
                        (CSS.CssClass "group" (Just ":hover") :| [CSS.CssClass "group-hover-red-text" Nothing])
                    )
                      :| []
                  )
                  "display: flex;",
                -- classes with special characters
                CSS.RuleGroup (singletonClass "camel-case-names" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "focus\\:-rotate-12" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "lg\\:hover\\:translate-y-1" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "bottom-0\\.5" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "bottom-1/2" Nothing :| []) "display: flex;",
                CSS.RuleGroup (singletonClass "pointer" (Just ":hover") :| []) "cursor: pointer;",
                -- multiple classes
                CSS.RuleGroup
                  ( singletonClass "class-1" (Just ":hover")
                      :| [singletonClass "class-2" Nothing, singletonClass "class-3" Nothing]
                  )
                  "cursor: pointer;",
                -- media queries
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.RuleGroup
                      (singletonClass "media-1" (Just ":hover") :| [])
                      "cursor: pointer;"
                  ],
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.MediaQuery
                      "(min-width: 320px)"
                      [CSS.RuleGroup (singletonClass "media-2" (Just ":hover") :| []) "cursor: pointer;"]
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
                "group;group",
                "groupHoverRedText;group-hover-red-text",
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
                "@keyframes spin-long {",
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
                  "spin-long"
                  [CSS.RuleGroup (CSS.GenericSelector "to" :| []) "\n    transform: rotate(360deg);\n  "],
                CSS.MediaQuery
                  "(min-width: 640px)"
                  [ CSS.RuleGroup
                      (singletonClass "sm\\:container" (Just " ") :| [])
                      "\n    width: 100%;\n  "
                  ],
                CSS.RuleGroup (singletonClass "-mx-3\\.5" (Just " ") :| []) "\n  margin-left: -0.875rem;\n  margin-right: -0.875rem;\n",
                CSS.RuleGroup (singletonClass "mt-0" (Just " ") :| []) "\n  margin-top: 0px;\n",
                CSS.MediaQuery
                  "(min-width: 1536px)"
                  [ CSS.MediaQuery "(min-width: 640px)" [CSS.RuleGroup (singletonClass "\\32xl\\:container" (Just " ") :| []) "\n      display: flex;\n    "],
                    CSS.RuleGroup (singletonClass "\\32xl\\:space-y-0" (Just " > :not([hidden]) ~ :not([hidden]) ") :| []) "\n    display: flex;\n  "
                  ]
              ]
      P.parse CSS.cssFile "cssFile" input === Right expected

  describe "CSS.ruleGroup & render @CSSNode" $
    it "node == (parse CSS.ruleGroup . render) node" $ do
      let body = "\n  display: flex;  margin-bottom: 10px;\n"
          selectors =
            [ singletonClass "some-class" Nothing :| [],
              singletonClass "some-class" (Just ":hover") :| [],
              singletonClass "some-class" (Just ":focus") :| [singletonClass "another-class" Nothing],
              singletonClass "bottom-1/2" Nothing :| [],
              singletonClass "focus\\:-rotate-12" Nothing :| [],
              singletonClass "bottom-0\\.5" Nothing :| [],
              singletonClass "lg\\:hover\\:translate-y-1" Nothing :| [],
              singletonClass "lg\\:hover\\:translate-y-1" Nothing :| [singletonClass "lg\\:hover\\:translate-y-1" Nothing]
            ]
          groups = fmap (`CSS.RuleGroup` body) selectors
          actual = fmap (P.parse CSS.ruleGroup "ruleGroup_render" . T.pack . render) groups
          expected = fmap Right groups
      mapM_ (uncurry shouldBe) $ zip actual expected
