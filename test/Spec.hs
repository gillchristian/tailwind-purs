{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Lib
import Test.Hspec
import qualified Text.Parsec as P

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

  describe "classesP" $
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
      P.parse classesP "classesP" input === Right ["minHScreen", "bgGray200", "container", "flex", "flexCol"]

  describe "cssToAvailableClasses" $
    it "transforms a css ast to available classes format" $ do
      let input =
            CssAST
              [ -- other nodes
                Comment " some comment ",
                RuleGroup (GenericSelector "body" :| []) "margin: 0;",
                RuleGroup (GenericSelector "*" :| []) "--tw-shadow: 0 0 #0000;",
                Comment " other comment ",
                -- duplicates
                RuleGroup (ClassSelector "flex" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "flex" Nothing :| []) "display: flex;",
                -- classes with special characters
                RuleGroup (ClassSelector "camel-case-names" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "focus\\:-rotate-12" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "bottom-0\\.5" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "bottom-1/2" Nothing :| []) "display: flex;",
                RuleGroup (ClassSelector "pointer" (Just ":hover") :| []) "cursor: pointer;",
                -- multiple classes
                RuleGroup
                  ( ClassSelector "class-1" (Just ":hover")
                      :| [ClassSelector "class-2" Nothing, ClassSelector "class-3" Nothing]
                  )
                  "cursor: pointer;",
                -- media queries
                MediaQuery
                  "(min-width: 640px)"
                  [ RuleGroup
                      (ClassSelector "media-1" (Just ":hover") :| [])
                      "cursor: pointer;"
                  ],
                MediaQuery
                  "(min-width: 640px)"
                  [ MediaQuery
                      "(min-width: 320px)"
                      [RuleGroup (ClassSelector "media-2" (Just ":hover") :| []) "cursor: pointer;"]
                  ],
                -- other queries
                Query
                  "keyframes"
                  "spin"
                  [RuleGroup (GenericSelector "to" :| []) "transform: rotate(360deg);"]
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
            CssAST
              [ Comment " a comment ",
                RuleGroup (GenericSelector "body" :| []) "\n  margin: 0;\n",
                RuleGroup (GenericSelector "b" :| [GenericSelector "strong"]) "\n  font-weight: bolder;\n",
                Comment " another\n   comment ",
                Query
                  "keyframes"
                  "spin"
                  [RuleGroup (GenericSelector "to" :| []) "\n    transform: rotate(360deg);\n  "],
                MediaQuery
                  "(min-width: 640px)"
                  [ RuleGroup
                      (ClassSelector "sm\\:container" Nothing :| [])
                      "\n    width: 100%;\n  "
                  ],
                RuleGroup (ClassSelector "-mx-3\\.5" Nothing :| []) "\n  margin-left: -0.875rem;\n  margin-right: -0.875rem;\n",
                RuleGroup (ClassSelector "mt-0" Nothing :| []) "\n  margin-top: 0px;\n",
                MediaQuery
                  "(min-width: 1536px)"
                  [ MediaQuery "(min-width: 640px)" [RuleGroup (ClassSelector "\\32xl\\:container" Nothing :| []) "\n      display: flex;\n    "],
                    RuleGroup (ClassSelector "\\32xl\\:space-y-0" (Just " > :not([hidden]) ~ :not([hidden])") :| []) "\n    display: flex;\n  "
                  ]
              ]
      P.parse cssFile "cssFile" input === Right expected

  describe "ruleGroup & renderNode" $
    it "node == (parse ruleGroup . renderNode) node" $ do
      let body = "\n  display: flex;  margin-bottom: 10px;\n"
          selectors =
            [ ClassSelector "some-class" Nothing :| [],
              ClassSelector "some-class" (Just ":hover") :| [],
              ClassSelector "some-class" (Just ":focus") :| [ClassSelector "another-class" Nothing],
              ClassSelector "bottom-1/2" Nothing :| [],
              ClassSelector "focus\\:-rotate-12" Nothing :| [],
              ClassSelector "bottom-0\\.5" Nothing :| [],
              ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| [],
              ClassSelector "lg\\:hover\\:translate-y-1" Nothing :| [ClassSelector "lg\\:hover\\:translate-y-1" Nothing]
            ]
          groups = fmap (flip RuleGroup body) selectors
          actual = fmap (P.parse ruleGroup "ruleGroup_renderNode" . T.pack . renderNode) groups
          expected = fmap Right groups
      mapM_ (uncurry shouldBe) $ zip actual expected
