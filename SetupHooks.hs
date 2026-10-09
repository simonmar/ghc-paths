-- | The locations of GHC that "GHC.Paths" is made from are given to the
-- library as @cpp-options@, when its component is configured.
module SetupHooks (setupHooks) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Directory (canonicalizePath)
import System.Process (readProcess)

import Distribution.Simple.Program (programPath)
import Distribution.Simple.Program.Db (lookupProgramByName)
import Distribution.Simple.SetupHooks
import qualified Distribution.Types.LocalBuildConfig as LBC

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks = noConfigureHooks{preConfComponentHook = Just preConfComponent}
    }

preConfComponent :: PreConfComponentHook
preConfComponent inputs@(PreConfComponentInputs lbc _ comp) = case comp of
  CLib _ -> do
    cppOpts <- ghcPathsOptions (LBC.withPrograms lbc)
    return $
      PreConfComponentOutputs $
        ComponentDiff $
          CLib emptyLibrary{libBuildInfo = emptyBuildInfo{cppOptions = cppOpts}}
  _ -> return (noPreConfComponentOutputs inputs)

ghcPathsOptions :: ProgramDb -> IO [String]
ghcPathsOptions progDb = do
  ghc <- required "ghc"
  ghcPkg <- required "ghc-pkg"
  libdir <- trim <$> readProcess ghc ["--print-libdir"] ""
  -- The documentation is where base's is, without its last component.
  baseHtml <- trim . takeWhile (/= '\n') <$> readProcess ghcPkg ["field", "base", "haddock-html", "--simple-output"] ""
  let docdir = fromMaybe baseHtml (reverseStripPrefix "/libraries/base" baseHtml)
  return
    [ "-DGHC_PATHS_GHC_PKG=" ++ show ghcPkg
    , "-DGHC_PATHS_GHC=" ++ show ghc
    , "-DGHC_PATHS_LIBDIR=" ++ show libdir
    , "-DGHC_PATHS_DOCDIR=" ++ show docdir
    ]
  where
    required name = case lookupProgramByName name progDb of
      Just prog -> canonicalizePath (programPath prog)
      Nothing -> ioError (userError (name ++ " was not found"))
    trim = dropWhileEnd isSpace . dropWhile isSpace
    reverseStripPrefix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)
