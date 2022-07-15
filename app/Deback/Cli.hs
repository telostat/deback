module Deback.Cli where

import Control.Applicative ((<**>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deback.Programs (doInitDisk, doListDisks, doMountDisk, doUnmountDisk, whenNotIdiot)
import Deback.Tools.Smartctl
  ( Disk (..)
  , DiskDevice (..)
  , listDisks
  )
import qualified Options.Applicative as OA
import qualified Text.Layout.Table as Table


commandParser :: OA.Parser (IO ())
commandParser =
  OA.subparser $
    ( OA.command "list-disks" (OA.info optDoListDisks (OA.progDesc listDisksProgDesc))
        <> OA.command
          "init-disk"
          ( OA.info optDoInitDisk (OA.progDesc "Encrypts and formats disk")
          )
        <> OA.command
          "mount-disk"
          ( OA.info optDoMountDisk (OA.progDesc "Mounts disk")
          )
        <> OA.command
          "unmount-disk"
          ( OA.info optDoUnmountDisk (OA.progDesc "Unmounts disk")
          )
    )


optDoListDisks :: OA.Parser (IO ())
optDoListDisks = pure doListDisks


optDoInitDisk :: OA.Parser (IO ())
optDoInitDisk =
  ( doInitDisk
      <$> OA.strOption
        ( OA.long "device"
            <> OA.metavar "DEVICE"
            <> OA.help "Device such as /dev/sda"
        )
        <*> OA.strOption
          ( OA.long "name"
              <> OA.metavar "NAME"
              <> OA.help "Device name to be mapped to"
          )
  )


optDoMountDisk :: OA.Parser (IO ())
optDoMountDisk =
  ( doMountDisk
      <$> OA.strOption
        ( OA.long "device"
            <> OA.metavar "DEVICE"
            <> OA.help "Device such as /dev/sda"
        )
        <*> OA.strOption
          ( OA.long "name"
              <> OA.metavar "NAME"
              <> OA.help "Device name to be mapped to"
          )
        <*> OA.strOption
          ( OA.long "path"
              <> OA.metavar "PATH"
              <> OA.help "Path to mount to"
          )
  )


optDoUnmountDisk :: OA.Parser (IO ())
optDoUnmountDisk =
  ( doUnmountDisk
      <$> OA.strOption
        ( OA.long "name"
            <> OA.metavar "NAME"
            <> OA.help "Device name to be unmounted"
        )
        <*> OA.strOption
          ( OA.long "path"
              <> OA.metavar "PATH"
              <> OA.help "Path to unmount from"
          )
  )


parser :: OA.ParserInfo (IO ())
parser =
  OA.info
    (commandParser <**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "See available commands."
        <> OA.header "deback - Backup and Archive Tool"
    )


listDisksProgDesc :: String
listDisksProgDesc =
  "List disks. \
  \WARNING: Note that this command may output unreliable information. \
  \USB enclosures and hubs may shadow real serial numbers, or indicate \
  \non-unique serial numbers for listed devices. Therefore, do not use \
  \this command in automated scripts. Also, running this command without \
  \super user privileges may result in errors or incomplete results."
