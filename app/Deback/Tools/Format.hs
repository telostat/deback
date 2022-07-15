module Deback.Tools.Format where

import qualified Data.Text as T
import qualified System.Process.Typed as TP


formatDisk :: T.Text -> IO ()
formatDisk name = TP.runProcess_ process
  where
    process = TP.proc "mkfs" ["-t", "ext4", "/dev/mapper/" <> T.unpack name]
