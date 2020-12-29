{-# LANGUAGE OverloadedStrings #-}

module Lib where

import AvailableClasses
import CLI
import qualified CSS
import Control.Applicative (Applicative (liftA2))
import qualified Control.Concurrent.STM as STM
import Control.Monad (join, unless, when)
import qualified Data.Bifunctor as BiF
import Data.Char (isNumber)
import Data.Default (def)
import qualified Data.Either as Either
import Data.Foldable (Foldable (fold))
import Data.List (intercalate, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Utils as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import qualified Options.Applicative as Opt
import qualified PureScript as PS
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path
import Text.Casing (camel)
import Text.Parsec.Text (parseFromFile)
import Text.Render
import Twitch ((|-), (|>))
import qualified Twitch
import Util

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
-- Business Logic
-- -----------------------------------------------------------------------------

extractClasses :: FilePath -> IO (Either String [String])
extractClasses path = do
  contents <- TextIO.readFile path
  if "import Tailwind as T" `Text.isInfixOf` contents
    then pure $ PS.usedTailwindClassNames path contents
    else pure $ Right []

extractUsedClasses :: [FilePath] -> IO (Either String [String])
extractUsedClasses files = fmap join . sequence <$> traverse extractClasses files

readAvailableClasses :: FilePath -> IO (Either String [CssClass])
readAvailableClasses path = BiF.first show <$> parseFromFile availableClasses path

parseInputCss :: FilePath -> IO (Either String CSS.AST)
parseInputCss path = BiF.first show <$> parseFromFile CSS.cssFile path

filterUsedClasses :: [String] -> [CssClass] -> [CssClass]
filterUsedClasses usedClasses =
  filter $ \(CssClass name _) -> name `elem` usedClasses

generateOnce :: PursClassesOptions -> Either String [CssClass] -> IO ()
generateOnce config classes = do
  cs <-
    if pursAll config
      then pure classes
      else do
        usedClasses <- extractUsedClasses =<< listFiles (pursSrc config)
        pure $ filterUsedClasses <$> usedClasses <*> classes
  generate config cs

generate :: PursClassesOptions -> Either String [CssClass] -> IO ()
generate config cs =
  case liftA2 (,) <$> fmap length <*> fmap PS.tailwindModule $ cs of
    Left err -> putStrLn err
    Right (count, contents) -> do
      TextIO.writeFile (pursOut config) contents
      unless (pursAll config) $ putStrLn $ "Found " <> show count <> " used classes"
      putStrLn $ List.replace (pursRoot config <> "/") "" (pursOut config) <> " updated succesfully!"

generateWatchMode :: PursClassesOptions -> Either String [CssClass] -> IO ()
generateWatchMode config classes = do
  generateOnce (config {pursAll = False}) classes
  files <- listFiles (pursSrc config)
  eFilesMap <- fmap Map.fromList . sequence <$> traverse extract files

  case eFilesMap of
    Left err -> putStrLn err
    Right filesMap -> do
      files <- STM.newTVarIO filesMap

      let conf = def {Twitch.root = Just $ pursSrc config, Twitch.usePolling = False}

      Twitch.defaultMainWithOptions conf $ do
        "./**/*.purs" |> \changedFile ->
          when (pursOut config /= changedFile) $ do
            extracedClasses <- extractClasses changedFile
            case extracedClasses of
              Left err -> do
                putStrLn $ "Failed to parse " <> changedFile
                putStrLn err
              Right newClasses -> do
                (didChange, usedClases) <- STM.atomically $ do
                  before <- sort . fold <$> STM.readTVar files
                  STM.modifyTVar files $ Map.insert changedFile newClasses
                  after <- sort . fold <$> STM.readTVar files
                  pure (before /= after, after)
                when didChange $ do
                  putStrLn $ List.replace (pursRoot config <> "/") "" changedFile <> " changed"
                  generate config $ filterUsedClasses usedClases <$> classes
                  putStrLn ""
        "./**/*.purs" |- \deletedFile ->
          when (pursOut config /= deletedFile) $ do
            (didChange, usedClases) <- STM.atomically $ do
              before <- sort . fold <$> STM.readTVar files
              STM.modifyTVar files $ Map.delete deletedFile
              after <- sort . fold <$> STM.readTVar files
              pure (before /= after, after)
            when didChange $ do
              putStrLn $ List.replace (pursRoot config <> "/") "" deletedFile <> " changed"
              generate config $ filterUsedClasses usedClases <$> classes
              putStrLn ""
  where
    extract :: FilePath -> IO (Either String (FilePath, [String]))
    extract path = fmap ((,) path) <$> extractClasses path

generatePursClasses :: PursClassesOptions -> IO ()
generatePursClasses config = do
  classes <- readAvailableClasses $ pursClasses config
  if pursWatch config
    then generateWatchMode config classes
    else generateOnce config classes

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
  classes <- readAvailableClasses $ cleanClasses config
  inputCss <- parseInputCss $ cleanInputCss config
  let outputCss = filterUnusedCss <$> (filterUsedClasses <$> usedClasses <*> classes) <*> inputCss
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
    Right classes -> do
      writeFile (genOut config) classes
      putStrLn $ "List of all the available classes written to " <> genOut config
    Left err -> putStrLn err

run :: IO ()
run = do
  cmd <- uncommand <$> Opt.execParser opts
  case cmd of
    GenAvaiableClasses opts -> generateAvailableClasses =<< normaliseGenAvailableClassesConfig opts
    PursClasses opts -> generatePursClasses =<< normalisePursClassesConfig opts
    CleanCss opts -> generateOptimizedCSS =<< normaliseCleanCssConfig opts
