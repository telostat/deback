module Deback.Tools.Mount where

import qualified Data.Text as T
import System.IO (stderr, stdout)
import qualified System.Process.Typed as TP


mount :: T.Text -> T.Text -> IO ()
mount name path = TP.runProcess_ process
  where
    process = TP.proc "mount" ["/dev/mapper/" <> T.unpack name, T.unpack path]


unmount :: T.Text -> IO ()
unmount path = TP.runProcess_ process
  where
    process = TP.proc "umount" [T.unpack path]
