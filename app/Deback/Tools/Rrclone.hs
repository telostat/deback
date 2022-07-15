module Deback.Tools.Rrclone where

import qualified System.Process.Typed as TP


runRrclone :: FilePath -> Bool -> IO ()
runRrclone config dryrun = TP.runProcess_ process
  where
    process = TP.proc "rrclone" (config : (if dryrun then ["--dry-run"] else []))
