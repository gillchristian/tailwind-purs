{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (join, unless)
import qualified Data.Bifunctor as BiF
import Data.Char (isNumber)
import qualified Data.Either as Either
import Data.List (intercalate, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Utils as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path
import Text.Casing (camel)
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser, parseFromFile)
import Text.Render
import Util

import qualified CSS

newtype FileName = FileName FilePath
  deriving (Show, Eq, Ord)

newtype FullPath = FullPath FilePath
  deriving (Show, Eq, Ord)

newtype Extension = Extension String
  deriving (Show, Eq, Ord)

data Directory
  = Directory FileName FullPath [File]
  deriving (Show, Eq, Ord)

data File
  = File FileName FullPath Extension
  deriving (Show, Eq, Ord)

traverseDirectory :: FilePath -> FilePath -> IO (Tree Directory)
traverseDirectory path name = do
  contents <- filter (not . isIgnoredPath) <$> Dir.getDirectoryContents path
  (files, dirs) <-
    normaliseItems . Maybe.catMaybes <$> traverse (toItem path) contents
  pure $ Node (Directory (FileName name) (FullPath path) files) dirs

normaliseItems :: [Either File (Tree Directory)] -> ([File], [Tree Directory])
normaliseItems = BiF.bimap (filter isPursFile) (filter $ not . isEmpty) . Either.partitionEithers

toItem :: FilePath -> FilePath -> IO (Maybe (Either File (Tree Directory)))
toItem rootPath name = do
  let path = Path.normalise $ rootPath </> name
  exists <- Dir.doesPathExist path
  isSym <- Dir.pathIsSymbolicLink path
  isDir <- Dir.doesDirectoryExist path
  case (not exists || isSym, isDir) of
    (True, _) -> pure Nothing
    (_, True) -> Just . Right <$> traverseDirectory path name
    (_, False) -> pure $ Just $ Left $ File (FileName name) (FullPath path) (takeExtension name)

isPursFile :: File -> Bool
isPursFile (File _ _ (Extension "purs")) = True
isPursFile _ = False

isEmpty :: Tree Directory -> Bool
isEmpty (Node (Directory _ _ []) []) = True
isEmpty _ = False

-- TODO: use .gitignore
isIgnoredPath :: FilePath -> Bool
isIgnoredPath "." = True
isIgnoredPath ".." = True
isIgnoredPath ".git" = True
isIgnoredPath "node_modules" = True
isIgnoredPath ".stack-work" = True
isIgnoredPath _ = False

takeExtension :: FilePath -> Extension
takeExtension path =
  case Path.takeExtension path of
    ('.' : extension) -> Extension extension
    extension -> Extension extension

filePath :: File -> FilePath
filePath (File _ (FullPath path) _) = path

dirFiles :: Directory -> [File]
dirFiles (Directory _ _ files) = files

getRoot :: FilePath -> IO (FilePath, String)
getRoot path =
  (,)
    <$> Dir.makeRelativeToCurrentDirectory path
    <*> fmap Path.takeFileName Dir.getCurrentDirectory

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
  Dir.setCurrentDirectory path
  (rootPath, rootName) <- getRoot path
  dir <- traverseDirectory rootPath rootName
  pure $ fmap filePath $ join $ Tree.flatten $ fmap dirFiles dir

-- -----------------------------------------------------------------------------

data CssClass = CssClass
  { classPursName :: String,
    classCssName :: String
  }
  deriving (Eq, Show, Ord)

instance Render CssClass where
  render (CssClass name css) =
    unlines
      [ "-- | " <> name,
        name <> " :: ClassName",
        name <> " = ClassName \"" <> css <> "\""
      ]

cssClass :: Parser CssClass
cssClass =
  CssClass
    <$> P.manyTill (P.noneOf ";") (P.char ';')
    <*> P.many1 (P.noneOf "\n")

classes :: Parser [CssClass]
classes = cssClass `P.endBy` P.spaces

-- -----------------------------------------------------------------------------
--
-- PureScript

data AST
  = TailwindClass String
  | CharNode
  deriving (Eq, Ord, Show)

node :: Parser AST
node = P.try classP <|> charNodeP

classP :: Parser AST
classP = TailwindClass <$> (P.string "T." *> P.many P.alphaNum)

charNodeP :: Parser AST
charNodeP = CharNode <$ P.anyChar

getClassName :: AST -> Maybe String
getClassName (TailwindClass name) = Just name
getClassName _ = Nothing

classesP :: Parser [String]
classesP = Maybe.mapMaybe getClassName <$> P.many node

extractClasses :: FilePath -> IO (Either String [String])
extractClasses path = do
  contents <- TextIO.readFile path
  if "import Tailwind as T" `Text.isInfixOf` contents
    then pure $ BiF.first show $ P.parse classesP path contents
    else pure $ Right []

-- -----------------------------------------------------------------------------

tailwindPurs :: [CssClass] -> Text
tailwindPurs usedClasses =
  Text.unlines
    [ "-- | Autogenerated from tailwind.css",
      "module Tailwind where",
      "",
      "import Halogen.HTML.Core (ClassName(..))",
      "",
      classes
    ]
  where
    classes = Text.intercalate "\n" $ map (Text.pack . render) usedClasses

filterUsedClasses :: [String] -> [CssClass] -> [CssClass]
filterUsedClasses usedClasses =
  filter $ \(CssClass name _) -> name `elem` usedClasses

-- -----------------------------------------------------------------------------

data PursClassesOptions = PursClassesOptions
  { pursRoot :: FilePath,
    pursSrc :: FilePath,
    pursClasses :: FilePath,
    pursOut :: FilePath,
    pursAll :: Bool
  }
  deriving (Eq, Show)

pursOpts :: Opt.ParserInfo Command
pursOpts =
  PursClasses <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Generate Tailwind.purs with only the used classes"
    opts =
      PursClassesOptions
        <$> Opt.strArgument
          ( Opt.metavar "ROOT"
              <> Opt.value "."
              <> Opt.help "Root directory of the project. The other paths are relative this path."
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "src"
              <> Opt.metavar "SRC"
              <> Opt.value "src"
              <> Opt.help "The source directory"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "classes"
              <> Opt.metavar "CLASSES"
              <> Opt.value "tailwind-classes.txt"
              <> Opt.help "File containing the available classes. This is the output from `$ twpurs gen-available-classes`"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "out"
              <> Opt.short 'o'
              <> Opt.metavar "OUT"
              <> Opt.value "src/Tailwind.purs"
              <> Opt.help "Output file"
              <> Opt.showDefault
          )
        <*> Opt.switch
          ( Opt.long "all"
              <> Opt.help "Generate all the available classes"
          )

data CleanCssOptions = CleanCssOptions
  { cleanRoot :: FilePath,
    cleanSrc :: FilePath,
    cleanClasses :: FilePath,
    cleanInputCss :: FilePath,
    cleanOut :: FilePath
  }
  deriving (Eq, Show)

cssOpts :: Opt.ParserInfo Command
cssOpts = CleanCss <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Generate tailwind.css with only the used styles"
    opts =
      CleanCssOptions
        <$> Opt.strArgument
          ( Opt.metavar "ROOT"
              <> Opt.value "."
              <> Opt.help "Root directory of the project"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "src"
              <> Opt.metavar "SRC"
              <> Opt.value "src"
              <> Opt.help "The directory with the PureScript source"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "classes"
              <> Opt.metavar "CLASSES"
              <> Opt.value "tailwind-classes.txt"
              <> Opt.help "File containing the available classes. This is the output from `$ twpurs gen-available-classes`"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "css"
              <> Opt.metavar "CSS"
              <> Opt.value "wip/tailwind.css"
              <> Opt.help "The file with all the Tailwind generated CSS"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "out"
              <> Opt.short 'o'
              <> Opt.metavar "OUT"
              <> Opt.value "dist/tailwind.css"
              <> Opt.help "Output file. This should be the production CSS file"
              <> Opt.showDefault
          )

data GenAvaiableClassesOptions = GenAvaiableClassesOptions
  { genRoot :: FilePath,
    genInputCss :: FilePath,
    genOut :: FilePath
  }
  deriving (Eq, Show)

genOpts :: Opt.ParserInfo Command
genOpts = GenAvaiableClasses <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Generate the list of available classes"
    opts =
      GenAvaiableClassesOptions
        <$> Opt.strArgument
          ( Opt.metavar "ROOT"
              <> Opt.value "."
              <> Opt.help "Root directory of the project"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "css"
              <> Opt.metavar "CSS"
              <> Opt.value "dev/tailwind.css"
              <> Opt.help "The file with all the Tailwind generated CSS"
              <> Opt.showDefault
          )
        <*> Opt.strOption
          ( Opt.long "out"
              <> Opt.short 'o'
              <> Opt.metavar "OUT"
              <> Opt.value "tailwind-classes.txt"
              <> Opt.help "Output file"
              <> Opt.showDefault
          )

data Command
  = PursClasses PursClassesOptions
  | CleanCss CleanCssOptions
  | GenAvaiableClasses GenAvaiableClassesOptions
  deriving (Eq, Show)

newtype Options = Options {uncommand :: Command}
  deriving (Eq, Show)

opts :: Opt.ParserInfo Options
opts =
  Opt.info
    (programm <**> Opt.helper)
    (Opt.fullDesc <> Opt.header "Do some Tailwind stuff")
  where
    programm = Options <$> Opt.hsubparser (gen <> purs <> css)
    gen = Opt.command "gen-available-classes" genOpts
    purs = Opt.command "gen-purs" pursOpts
    css = Opt.command "gen-css" cssOpts

mkDefaultGenAvailableClassesConfig :: FilePath -> GenAvaiableClassesOptions
mkDefaultGenAvailableClassesConfig root =
  GenAvaiableClassesOptions
    { genRoot = root,
      genInputCss = root </> "wip/tailwind.css",
      genOut = root </> "available-classes.txt"
    }

normaliseGenAvailableClassesConfig :: GenAvaiableClassesOptions -> IO GenAvaiableClassesOptions
normaliseGenAvailableClassesConfig (GenAvaiableClassesOptions root css out) = do
  root <- Dir.makeAbsolute root
  pure $
    GenAvaiableClassesOptions
      { genRoot = root,
        genInputCss = root </> css,
        genOut = root </> out
      }

mkDefaultPursClassesOptions :: FilePath -> PursClassesOptions
mkDefaultPursClassesOptions root =
  PursClassesOptions
    { pursRoot = root,
      pursClasses = root </> "css.txt",
      pursSrc = root </> "src",
      pursOut = root </> "src/Tailwind.purs",
      pursAll = False
    }

normalisePursClassesConfig :: PursClassesOptions -> IO PursClassesOptions
normalisePursClassesConfig (PursClassesOptions root src classes out all) = do
  root <- Dir.makeAbsolute root
  pure $
    PursClassesOptions
      { pursRoot = root,
        pursClasses = root </> classes,
        pursSrc = root </> src,
        pursOut = root </> out,
        pursAll = all
      }

mkDefaultCleanCssOptions :: FilePath -> CleanCssOptions
mkDefaultCleanCssOptions root =
  CleanCssOptions
    { cleanRoot = root,
      cleanSrc = root </> "src",
      cleanClasses = root </> "css.txt",
      cleanInputCss = root </> "wip/tailwind.css",
      cleanOut = root </> "public/tailwind.css"
    }

normaliseCleanCssConfig :: CleanCssOptions -> IO CleanCssOptions
normaliseCleanCssConfig (CleanCssOptions root src classes css out) = do
  root <- Dir.makeAbsolute root
  pure $
    CleanCssOptions
      { cleanRoot = root,
        cleanSrc = root </> src,
        cleanClasses = root </> classes,
        cleanInputCss = root </> css,
        cleanOut = root </> out
      }

-- -----------------------------------------------------------------------------

extractUsedClasses :: [FilePath] -> IO (Either String [String])
extractUsedClasses files = fmap join . sequence <$> traverse extractClasses files

readAvailableClasses :: FilePath -> IO (Either String [CssClass])
readAvailableClasses path = BiF.first show <$> parseFromFile classes path

parseInputCss :: FilePath -> IO (Either String CSS.AST)
parseInputCss path = BiF.first show <$> parseFromFile CSS.cssFile path

generatePursClasses :: PursClassesOptions -> IO ()
generatePursClasses config = do
  availableClasses <- readAvailableClasses $ pursClasses config
  cs <-
    if pursAll config
      then pure availableClasses
      else do
        usedClasses <- extractUsedClasses =<< listFiles (pursSrc config)
        pure $ filterUsedClasses <$> usedClasses <*> availableClasses
  case liftA2 (,) <$> fmap length <*> fmap tailwindPurs $ cs of
    Right (count, contents) -> do
      TextIO.writeFile (pursOut config) contents
      unless (pursAll config) $ putStrLn $ "Found " <> show count <> " used classes"
      putStrLn $ pursOut config <> " updated succesfully!"
    Left err -> putStrLn err

filterUnusedCss :: [CssClass] -> CSS.AST -> CSS.AST
filterUnusedCss used (CSS.AST nodes) = CSS.AST $ Maybe.mapMaybe mapFilterNode nodes
  where
    selectorMatchesClass :: CSS.Selector -> CssClass -> Bool
    selectorMatchesClass (CSS.ClassSelector a' _) (CssClass _ a) = filter (/= '\\') a' == a
    selectorMatchesClass _ _ = False

    isGenericSelector :: CSS.Selector -> Bool
    isGenericSelector (CSS.GenericSelector _) = True
    isGenericSelector _ = False

    -- TODO: simplify filtering logic
    mapFilterNode :: CSS.CssNode -> Maybe CSS.CssNode
    mapFilterNode node@(CSS.RuleGroup selectors _) =
      if any (\selector -> isGenericSelector selector || any (selectorMatchesClass selector) used) selectors
        then Just node
        else Nothing
    mapFilterNode (CSS.Comment _) = Nothing
    mapFilterNode node@CSS.Query {} = Just node
    mapFilterNode (CSS.MediaQuery q children) =
      if not . null $ filtered
        then Just $ CSS.MediaQuery q filtered
        else Nothing
      where
        filtered = CSS.unAst $ filterUnusedCss used $ CSS.AST children

generateOptimizedCSS :: CleanCssOptions -> IO ()
generateOptimizedCSS config = do
  usedClasses <- extractUsedClasses =<< listFiles (cleanSrc config)
  availableClasses <- readAvailableClasses $ cleanClasses config
  inputCss <- parseInputCss $ cleanInputCss config
  let outputCss = filterUnusedCss <$> (filterUsedClasses <$> usedClasses <*> availableClasses) <*> inputCss
  case outputCss of
    Right css -> do
      -- TODO stats (eg. size before & after)
      writeFile (cleanOut config) $ render css
      putStrLn $ "Optimized CSS written to " <> cleanOut config
    Left err -> putStrLn err

className (CSS.GenericSelector _) = Nothing
className (CSS.ClassSelector class' _) = Just class'

nodeClasses :: CSS.CssNode -> [String]
nodeClasses (CSS.RuleGroup selectors _) = Maybe.mapMaybe className $ NE.toList selectors
nodeClasses (CSS.MediaQuery _ nodes) = nodeClasses =<< nodes
nodeClasses _ = []

cssToPursName :: String -> String
cssToPursName =
  num
    . camel
    . neg
    . filter (/= '\\')
    . replace '/' 'd'
    . replace ':' '-'
    . replace '.' 'p'
    . List.replace ":-" "-neg-"
  where
    neg s = if startsWith (== '-') s then "neg-" ++ s else s
    num s = if startsWith isNumber s then '_' : s else s

escapeCssName :: String -> String
escapeCssName = filter (/= '\\')

pursifyCssClass :: String -> CssClass
pursifyCssClass = CssClass <$> cssToPursName <*> escapeCssName

pursAndCss :: CssClass -> String
pursAndCss cx = classPursName cx <> ";" <> classCssName cx

cssToAvailableClasses :: CSS.AST -> String
cssToAvailableClasses =
  intercalate "\n"
    . sort
    . fmap (pursAndCss . pursifyCssClass)
    . Set.toList
    . Set.fromList
    . (nodeClasses =<<)
    . CSS.unAst

generateAvailableClasses :: GenAvaiableClassesOptions -> IO ()
generateAvailableClasses config = do
  inputCss <- parseInputCss (genInputCss config)
  case cssToAvailableClasses <$> inputCss of
    Right availableClasses -> do
      writeFile (genOut config) availableClasses
      putStrLn $ "List of all the available classes written to " <> genOut config
    Left err -> putStrLn err

run :: IO ()
run = do
  cmd <- uncommand <$> Opt.execParser opts
  case cmd of
    GenAvaiableClasses opts -> generateAvailableClasses =<< normaliseGenAvailableClassesConfig opts
    PursClasses opts -> generatePursClasses =<< normalisePursClassesConfig opts
    CleanCss opts -> generateOptimizedCSS =<< normaliseCleanCssConfig opts
