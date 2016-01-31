#!/usr/bin/env stack
-- stack --install-ghc runghc --package filepath --package split --package temporary --package MissingH --package diff-parse --package text

{-# LANGUAGE RecordWildCards, ViewPatterns #-}

import Control.Monad
import Control.Exception
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import Data.String.Utils
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Posix
import System.Exit
import System.IO
import System.IO.Temp
import Text.Diff.Parse
import Text.Diff.Parse.Types

import qualified Data.Text as Text

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

    mapM ((\cmd -> print cmd >> system cmd) . runTestCmd) cmds

--    rawSystem "find" [i]

    (rv, diff, err)  <- readProcessWithExitCode "git" ["diff", "--no-index", "--color", "--word-diff=color", "--", e, i] ""

    (rv', plaindiff, err')  <- readProcessWithExitCode "git" ["diff", "--no-index", "--", e, i] ""
    plaindiff' <- fixdiff common test_dir e i plaindiff

    case rv of
      ExitSuccess -> return True
      ExitFailure _ -> do
              let [_, g, t] = splitDirectories test_dir
              createDirectoryIfMissing True "diffs"
              writeFile ("diffs" </> (g ++ "; " ++ t) <.> "diff") $ removeGitMetadata plaindiff'
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


fixdiff _ _ _ _ plaindiff | strip plaindiff == "" = return ""
fixdiff common test_dir e i plaindiff = do
  let sdss = case parseDiff $ Text.pack plaindiff of
              Left e -> error $ "fixdiff: " ++ e
              Right fds -> flip map fds $ \FileDelta {..} ->
                let src = Text.unpack fileDeltaSourceFile
                    dst = Text.unpack fileDeltaDestFile
                in
                  [src, dst]
      sds = map ("/" </>) $ concat sdss

  plaindiff' <- flip execStateT plaindiff $ forM sds $ \sd -> do
    let (joinPath -> d', joinPath -> f) = splitAt 3 $ splitPath sd
        -- d | d' `equalFilePath` i = "in"
        --   | d' `equalFilePath` e = "ex"
        d = "ex"

    cd <- liftIO $ doesFileExist $ common </> d </> f
    td <- liftIO $ doesFileExist $ test_dir </> d </> f

    let r
         | cd = common </> d </> f
         | td = test_dir </> d </> f
         | otherwise = test_dir </> d </> f

    put =<< (replace sd ("/" </> r) <$> get)

  return plaindiff'

removeGitMetadata = unlines . filter (\l -> not $ "index" `isPrefixOf` l || "diff" `isPrefixOf` l || "new" `isPrefixOf` l) . lines
