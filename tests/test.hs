#!/usr/bin/env stack
-- stack --install-ghc runghc --package filepath --package split --package temporary

import Control.Monad
import Control.Exception
import Data.List
import Data.List.Split
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Posix
import System.Exit
import System.IO
import System.IO.Temp


scriptPath =
    head . drop 1 . reverse . splitOn "\NUL" <$> readFile "/proc/self/cmdline"

ls :: FilePath -> IO [FilePath]
ls dir = do
  filter (not . (`elem` [".", ".."])) <$> getDirectoryContents dir

shellEscape str = intercalate "'\"'\"'" $ splitOn "'" str

kib = "../dist/build/kib/kib"

main = do
  ds <- filterM ((isDirectory <$>) . getFileStatus . ("data"</>)) =<< ls "data"
  let tests = filter (not . (`elem` ["common"])) ds
  let common = "data" </> "common"

  res <- forM tests $ \test -> withSystemTempDirectory "kib" $ \tmp -> do
    hPutStrLn stderr $ " - " ++ test
    let tsrc = "data" </> test
        tdir = tmp </> test
        indir = tsrc </> "in"
        outdir = tsrc </> "ex"

    in_exists <- doesDirectoryExist indir

    rawSystem "cp" ["-r", common </> "in" </> ".", tdir]

    when in_exists $
         void $ rawSystem "cp" ["-r", indir </> ".", tmp </> test]

--    rawSystem "ls" ["-lR", tdir]
    let cmds = splitOn ";" test
        runTestCmd cmd = kib ++ " --root '"++ shellEscape tdir ++ "' " ++ cmd

    mapM (system . runTestCmd) cmds

    (rv, diff, err)  <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", "--word-diff=color", "--", outdir, tdir] ""

    case rv of
      ExitSuccess -> return True
      ExitFailure _ -> do
              putStrLn diff
              putStrLn ""
              return False

  if and res
     then exitSuccess
     else exitFailure
