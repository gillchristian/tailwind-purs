module Lib
  ( run,
  )
where

import Control.Monad (join, void)
import Data.Bifunctor (bimap)
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.FilePath as Path
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser, parseFromFile)

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
normaliseItems = bimap (filter isPursFile) (filter $ not . isEmpty) . Either.partitionEithers

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

runFoo :: FilePath -> IO ()
runFoo path = do
  Dir.setCurrentDirectory path
  (rootPath, rootName) <- getRoot path
  dir <- traverseDirectory rootPath rootName
  mapM_ (putStrLn . filePath) $ join $ Tree.flatten $ fmap dirFiles dir

headOr :: a -> [a] -> a
headOr a [] = a
headOr _ (a : _) = a

-- -----------------------------------------------------------------------------

data CssClass
  = CssClass String String
  deriving (Show, Eq, Ord)

cssClass :: Parser CssClass
cssClass =
  CssClass
    <$> P.manyTill P.alphaNum (P.char ';')
    <*> P.many1 (P.alphaNum <|> P.oneOf "-:/")

classes :: Parser [CssClass]
classes = cssClass `P.endBy` P.spaces

run :: IO ()
run = do
  runFoo =<< headOr "/home/bb8/dev/fpers/src" <$> Env.getArgs
  result <- parseFromFile classes "/home/bb8/dev/fpers/css.txt"
  print $ take 5 <$> result
  print $ last <$> result
  print $ length <$> result
