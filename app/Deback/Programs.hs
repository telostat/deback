{-# LANGUAGE OverloadedStrings #-}

module Deback.Programs where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deback.Smartctl
  ( Disk (..)
  , DiskDevice (..)
  , listDisks
  )
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
      , show $ floor $ ((fromRational $ toRational $ diskUserCapacity d) / 1024 / 1024 / 1024)
      ]
