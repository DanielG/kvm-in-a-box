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
  let common = "data" </> "common"
      test_groups = [ "data/clean-system"
                    , "data/full-system"
                    ]

  tdirs <- forM test_groups $ \gdir ->
    filterM ((isDirectory <$>) . getFileStatus) =<< (map (gdir</>) <$> ls gdir)

  let tests = concat tdirs

  res <- forM tests $ \test_dir -> withTdirs $ \i e -> do
    putStrLn $ " ------------------------- " ++ test_dir ++ " -------------------------"
    let indir = test_dir </> "in"
        exdir = test_dir </> "ex"

    in_exists <- doesDirectoryExist indir
    ex_exists <- doesDirectoryExist exdir

    rawSystem "cp" ["-r", common </> "in" </> ".", i]
    rawSystem "cp" ["-r", common </> "ex" </> ".", e]

    when in_exists $
         void $ rawSystem "cp" ["-r", indir </> ".", i]

    when ex_exists $
         void $ rawSystem "cp" ["-r", exdir </> ".", e]

    let cmds = splitOn ";" $ takeFileName test_dir
        runTestCmd cmd = kib ++ " --root '"++ shellEscape i ++ "' " ++ cmd

    mapM (system . runTestCmd) cmds

--    rawSystem "find" [i]

    (rv, diff, err)  <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", "--word-diff=color", "--", e, i] ""

    case rv of
      ExitSuccess -> return True
      ExitFailure _ -> do
              putStrLn diff
              putStrLn ""
              return False

  if and res
     then exitSuccess
     else exitFailure

withTdirs :: (FilePath -> FilePath -> IO a) -> IO a
withTdirs f = do
  withSystemTempDirectory "kib-in" $ \i ->
    withSystemTempDirectory "kib-ex" $ \e ->
      f i e
 where
   withSystemTempDirectory = withT

withT tpl a = do
  tmp <- getTemporaryDirectory
  dir <- createTempDirectory tmp tpl
  a dir
