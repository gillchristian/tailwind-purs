module Lib
  ( run,
  )
where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
  = Directory FileName FullPath [Directory] [File]
  deriving (Show, Eq, Ord)

data File
  = File FileName FullPath Extension
  deriving (Show, Eq, Ord)

traverseDirectory :: FilePath -> FilePath -> IO Directory
traverseDirectory path name = do
  contents <- filter (not . isIgnoredPath) <$> Dir.getDirectoryContents path
  (files, dirs) <-
    normaliseItems . Maybe.catMaybes <$> traverse (toItem path) contents
  pure $ Directory (FileName name) (FullPath path) dirs files

normaliseFiles :: [File] -> [File]
normaliseFiles = List.sort . filter isPursFile

normaliseDirs :: [Directory] -> [Directory]
normaliseDirs = List.sort . filter (not . isEmpty)

normaliseItems :: [Either File Directory] -> ([File], [Directory])
normaliseItems = bimap normaliseFiles normaliseDirs . Either.partitionEithers

toItem :: FilePath -> FilePath -> IO (Maybe (Either File Directory))
toItem rootPath name = do
  let path = Path.normalise $ rootPath </> name
  exists <- Dir.doesPathExist path
  isSym <- Dir.pathIsSymbolicLink path
  isDir <- Dir.doesDirectoryExist path
  if not exists || isSym
    then pure Nothing
    else
      if isDir
        then Just . Right <$> traverseDirectory path name
        else pure $ Just $ Left $ File (FileName name) (FullPath path) (takeExtension name)

isPursFile :: File -> Bool
isPursFile (File _ _ (Extension "purs")) = True
isPursFile _ = False

isEmpty :: Directory -> Bool
isEmpty (Directory _ _ [] []) = True
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

listFiles :: Directory -> [File]
listFiles (Directory _ _ dirs files) = join (map listFiles dirs) ++ files

filePath :: File -> FilePath
filePath (File _ (FullPath path) _) = path

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
  mapM_ (putStrLn . filePath) $ listFiles dir

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
