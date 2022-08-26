{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Deback.Programs where

import Control.Exception (IOException, catch)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deback.Tools.Autorestic (runAutoresticBackup, runAutoresticCheck, runAutoresticInfo, runAutoresticSnapshots)
import Deback.Tools.Cryptsetup (luksClose, luksFormat, luksOpen)
import Deback.Tools.Format (formatDisk)
import Deback.Tools.Mount (mount, unmount)
import Deback.Tools.Rrclone (runRrclone)
import Deback.Tools.Smartctl
  ( Disk (..)
  , DiskDevice (..)
  , listDisks
  )
import qualified System.Console.ANSI as Ansi
import System.IO (hFlush, stdout)
import qualified System.Process.Typed as TP
import qualified Text.Layout.Table as Table


doListDisks :: IO ()
doListDisks = do
  disks <- listDisks
  let table =
        Table.tableString
          [Table.def, Table.def, Table.def, Table.def, Table.numCol]
          Table.unicodeS
          (Table.titlesH ["Device", "Model", "Serial", "Firmware", "Capacity (GB)"])
          (map (Table.rowG . mkRow) disks)
  putStrLn table
  where
    mkRow d =
      [ T.unpack $ diskDeviceName $ diskDevice d
      , T.unpack $ fromMaybe "" $ diskModelName d
      , T.unpack $ fromMaybe "" $ diskSerialNumber d
      , T.unpack $ fromMaybe "" $ diskFirmwareVersion d
      , show $ floor (fromRational (toRational $ diskUserCapacity d) / 1024 / 1024 / 1024)
      ]


doInitDisk :: T.Text -> T.Text -> IO ()
doInitDisk device name = whenNotIdiot $ do
  putStrLn "LUKS formatting..."
  luksFormat device
  putStrLn "LUKS opening..."
  luksOpen device name
  putStrLn "Formatting..."
  formatDisk name
  putStrLn "LUKS closing..."
  luksClose name


doMountDisk :: T.Text -> T.Text -> T.Text -> IO ()
doMountDisk device name path = whenNotIdiot $ do
  putStrLn "LUKS opening..."
  luksOpen device name
  putStrLn "Mounting..."
  mount name path


doUnmountDisk :: T.Text -> T.Text -> IO ()
doUnmountDisk name path = whenNotIdiot $ do
  putStrLn "Unmounting..."
  unmount path `catch` (\(e :: TP.ExitCodeException) -> pure ())
  putStrLn "LUKS closing..."
  luksClose name


doSync :: FilePath -> Bool -> IO ()
doSync = runRrclone


doBackupCheck :: FilePath -> IO ()
doBackupCheck = runAutoresticCheck


doBackupInfo :: FilePath -> IO ()
doBackupInfo = runAutoresticInfo


doBackupRun :: FilePath -> IO ()
doBackupRun config = runAutoresticCheck config >> runAutoresticBackup config


doBackupSnapshots :: FilePath -> IO ()
doBackupSnapshots = runAutoresticSnapshots


doSyncAndBackup :: FilePath -> FilePath -> IO ()
doSyncAndBackup configSync configBackup = doSync configSync False >> doBackupRun configBackup


whenNotIdiot :: IO () -> IO ()
whenNotIdiot act = do
  let check = "I am not an idiot!"
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red]
  putStr $ "DANGER! This is a destructive operations. Please type \"" <> check <> "\" and hit [ENTER] to continue: "
  hFlush stdout
  Ansi.setSGR [Ansi.Reset]
  input <- getLine
  if check /= input
    then putStrLn "Aborting!"
    else putStrLn "Continuing..." >> act
