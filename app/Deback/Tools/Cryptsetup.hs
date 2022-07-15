{-# LANGUAGE OverloadedStrings #-}

module Deback.Tools.Cryptsetup where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process.Typed as TP


luksFormat :: T.Text -> IO ()
luksFormat device = TP.runProcess_ process
  where
    process = TP.proc "cryptsetup" ["luksFormat", T.unpack device]


luksOpen :: T.Text -> T.Text -> IO ()
luksOpen device name = TP.runProcess_ process
  where
    process = TP.proc "cryptsetup" ["luksOpen", T.unpack device, T.unpack name]


luksClose :: T.Text -> IO ()
luksClose name = TP.runProcess_ process
  where
    process = TP.proc "cryptsetup" ["luksClose", "/dev/mapper/" <> T.unpack name]
