
import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Control.Monad.Error ( catchError )
import Data.List ( isSuffixOf )

import System.Directory ( getDirectoryContents, copyFile )
import System.FilePath ( (</>) )
import System.Process ( rawSystem )

import Distribution.PackageDescription ( PackageDescription(..) )
import Distribution.Simple ( UserHooks(..), defaultMainWithHooks, simpleUserHooks )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), absoluteInstallDirs, bindir )
import Distribution.Simple.Setup ( fromFlag, copyDest, CopyDest(..) )


main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
        { postCopy = \ _ flags pkg lbi -> copyScripts pkg lbi (fromFlag $ copyDest flags)
        , postInst = \ _ _     pkg lbi -> copyScripts pkg lbi NoCopyDest
        }

copyScripts :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
copyScripts pkg local copy = do
    let dirs = absoluteInstallDirs pkg local copy
    -- copyFile "scripts/run/conjure-server" (bindir dirs </> "conjure-server")
    ruleFiles <- filter (\ f -> ".rule" `isSuffixOf` f || ".repr" `isSuffixOf` f ) <$> allFiles "files/rules"
    void $ rawSystem "conjureBF" ("makeRulesDB" : ruleFiles)

allFiles :: FilePath -> IO [FilePath]
allFiles x = do
    let dots i = not ( i == "." || i == ".." )
    ys' <- getDirectoryContents x `catchError` const (return [])
    let ys = filter dots ys'
    if null ys
        then return [x]
        else ((x :) . concat) <$> mapM (allFiles . (x </>)) ys

