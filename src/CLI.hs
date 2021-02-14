module CLI where

import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified System.Directory as Dir
import System.FilePath ((</>))

data PursClassesOptions = PursClassesOptions
  { pursRoot :: FilePath,
    pursSrc :: FilePath,
    pursClasses :: FilePath,
    pursOut :: FilePath,
    pursAll :: Bool,
    pursWatch :: Bool
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
        <*> Opt.switch
          ( Opt.long "watch"
              <> Opt.short 'w'
              <> Opt.help "Run in watch mode. `--all` is ingored."
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

data HtmlToPursOptions = HtmlToPursOptions
  { htmlArgs :: [String],
    htmlSingleLine :: Bool
  }
  deriving (Eq, Show)

htmlOpts :: Opt.ParserInfo Command
htmlOpts = HtmlToPurs <$> Opt.info opts (Opt.progDesc desc)
  where
    desc = "Parse a list of CSS classes (from the HTML class attribute: `<div class=\"foo bar baz\">`) and produce the Halogen PureScript version `[ T.foo, T.bar, T.baz ]`"
    opts =
      HtmlToPursOptions
        <$> Opt.many (Opt.strArgument $ Opt.metavar "class")
        <*> Opt.switch
          ( Opt.long "single-line"
              <> Opt.short 's'
              <> Opt.help "Output in single line"
          )

data Command
  = PursClasses PursClassesOptions
  | CleanCss CleanCssOptions
  | GenAvaiableClasses GenAvaiableClassesOptions
  | HtmlToPurs HtmlToPursOptions
  deriving (Eq, Show)

newtype Options = Options {uncommand :: Command}
  deriving (Eq, Show)

opts :: Opt.ParserInfo Options
opts =
  Opt.info
    (programm <**> Opt.helper)
    (Opt.fullDesc <> Opt.header "Do some Tailwind stuff")
  where
    programm = Options <$> Opt.hsubparser (gen <> purs <> css <> html)
    gen = Opt.command "gen-available-classes" genOpts
    purs = Opt.command "gen-purs" pursOpts
    css = Opt.command "gen-css" cssOpts
    html = Opt.command "html2purs" htmlOpts

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
      pursAll = False,
      pursWatch = False
    }

normalisePursClassesConfig :: PursClassesOptions -> IO PursClassesOptions
normalisePursClassesConfig (PursClassesOptions root src classes out all watch) = do
  root <- Dir.makeAbsolute root
  pure $
    PursClassesOptions
      { pursRoot = root,
        pursClasses = root </> classes,
        pursSrc = root </> src,
        pursOut = root </> out,
        pursAll = all,
        pursWatch = watch
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
