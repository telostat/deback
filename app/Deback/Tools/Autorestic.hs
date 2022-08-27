module Deback.Tools.Autorestic where

import qualified System.Process.Typed as TP


runAutoresticCheck :: FilePath -> IO ()
runAutoresticCheck config = TP.runProcess_ process
  where
    process = TP.proc "autorestic" ["--config", config, "--ci", "check"]


runAutoresticInfo :: FilePath -> IO ()
runAutoresticInfo config = TP.runProcess_ process
  where
    process = TP.proc "autorestic" ["--config", config, "--ci", "info"]


runAutoresticBackup :: FilePath -> IO ()
runAutoresticBackup config = TP.runProcess_ process
  where
    process = TP.proc "autorestic" ["--config", config, "--ci", "backup", "--all"]


runAutoresticSnapshots :: FilePath -> IO ()
runAutoresticSnapshots config = TP.runProcess_ process
  where
    process = TP.proc "autorestic" ["--config", config, "--ci", "exec", "--all", "--", "snapshots"]
