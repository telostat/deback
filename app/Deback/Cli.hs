module Deback.Cli where

import Data.Version (showVersion)
import qualified Deback.Programs as Programs
import qualified Options.Applicative as OA
import Paths_deback (version)


parser :: OA.ParserInfo (IO ())
parser =
  OA.info
    (OA.helper <*> versionOptParser <*> commandParser)
    ( OA.fullDesc
        <> OA.progDesc "See available commands."
        <> OA.header ("deback - Some File Synchronisation and Backup Tool v" <> showVersion version)
    )


versionOptParser :: OA.Parser (a -> a)
versionOptParser = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")


commandParser :: OA.Parser (IO ())
commandParser =
  OA.subparser $
    OA.command "list-disks" (OA.info optDoListDisks progDescListDisks)
      <> OA.command "init-disk" (OA.info optDoInitDisk progDescInitDisk)
      <> OA.command "mount-disk" (OA.info optDoMountDisk progDescMountDisk)
      <> OA.command "unmount-disk" (OA.info optDoUnmountDisk progDescUnmountDisk)
      <> OA.command "sync" (OA.info optDoSync progDescSync)
      <> OA.command "backup-check" (OA.info optDoBackupCheck progDescBackupCheck)
      <> OA.command "backup-info" (OA.info optDoBackupInfo progDescBackupInfo)
      <> OA.command "backup-run" (OA.info optDoBackupRun progDescBackupRun)
      <> OA.command "backup-snapshots" (OA.info optDoBackupSnapshots progDescBackupSnapshots)
      <> OA.command "sync-and-backup" (OA.info optDoSyncAndBackup progDescSyncAndBackup)


optDoListDisks :: OA.Parser (IO ())
optDoListDisks = pure Programs.doListDisks


progDescListDisks :: OA.InfoMod a
progDescListDisks =
  OA.progDesc
    "Lists disks. \
    \WARNING: Note that this command may output unreliable information. \
    \USB enclosures and hubs may shadow real serial numbers, or indicate \
    \non-unique serial numbers for listed devices. Therefore, do not use \
    \this command in automated scripts. Also, running this command without \
    \super user privileges may result in errors or incomplete results."


optDoInitDisk :: OA.Parser (IO ())
optDoInitDisk =
  Programs.doInitDisk
    <$> OA.strOption (OA.long "device" <> OA.metavar "DEVICE" <> OA.help "Device such as \"/dev/sda\"")
    <*> OA.strOption (OA.long "name" <> OA.metavar "NAME" <> OA.help "Device name to be mapped to such as \"my-green-disk\"")


progDescInitDisk :: OA.InfoMod a
progDescInitDisk =
  OA.progDesc
    "Encrypts and formats a disk. \
    \Note that you need to have super user privileges to run this command."


optDoMountDisk :: OA.Parser (IO ())
optDoMountDisk =
  Programs.doMountDisk
    <$> OA.strOption (OA.long "device" <> OA.metavar "DEVICE" <> OA.help "Device such as \"/dev/sda\"")
    <*> OA.strOption (OA.long "name" <> OA.metavar "NAME" <> OA.help "Device name to be mounted such as \"my-green-disk\"")
    <*> OA.strOption (OA.long "path" <> OA.metavar "PATH" <> OA.help "Path to the mount point")


progDescMountDisk :: OA.InfoMod a
progDescMountDisk =
  OA.progDesc
    "Mounts a disk. \
    \Note that you need to have super user privileges to run this command."


optDoUnmountDisk :: OA.Parser (IO ())
optDoUnmountDisk =
  Programs.doUnmountDisk
    <$> OA.strOption (OA.long "name" <> OA.metavar "NAME" <> OA.help "Device name to be unmounted such as \"my-green-disk\"")
    <*> OA.strOption (OA.long "path" <> OA.metavar "PATH" <> OA.help "Path to the mount point")


progDescUnmountDisk :: OA.InfoMod a
progDescUnmountDisk =
  OA.progDesc
    "Unmounts a disk. \
    \Note that you need to have super user privileges to run this command."


optDoSync :: OA.Parser (IO ())
optDoSync =
  Programs.doSync
    <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")
    <*> OA.switch (OA.long "dry-run" <> OA.help "Dry-run (ie. no actual syncing will take place)")


progDescSync :: OA.InfoMod a
progDescSync =
  OA.progDesc
    "Runs the sync process. \
    \Note that you most probably do not want to be super user when running \
    \this command."


optDoBackupCheck :: OA.Parser (IO ())
optDoBackupCheck =
  Programs.doBackupCheck
    <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")


progDescBackupCheck :: OA.InfoMod a
progDescBackupCheck =
  OA.progDesc
    "Checks the backup configuration. \
    \Note that you most probably do not want to be super user when running \
    \this command."


optDoBackupInfo :: OA.Parser (IO ())
optDoBackupInfo =
  Programs.doBackupInfo
    <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")


progDescBackupInfo :: OA.InfoMod a
progDescBackupInfo =
  OA.progDesc
    "Displays backup plan. \
    \Note that you most probably do not want to be super user when running \
    \this command."


optDoBackupRun :: OA.Parser (IO ())
optDoBackupRun =
  Programs.doBackupRun
    <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")


progDescBackupRun :: OA.InfoMod a
progDescBackupRun =
  OA.progDesc
    "Runs the backup process. \
    \Note that you most probably do not want to be super user when running \
    \this command."


optDoBackupSnapshots :: OA.Parser (IO ())
optDoBackupSnapshots =
  Programs.doBackupSnapshots
    <$> OA.strOption (OA.long "config" <> OA.metavar "CONFIG-FILE" <> OA.help "Path to configuration file")


progDescBackupSnapshots :: OA.InfoMod a
progDescBackupSnapshots =
  OA.progDesc
    "Shows backup snapshots. \
    \Note that you most probably do not want to be super user when running \
    \this command."


optDoSyncAndBackup :: OA.Parser (IO ())
optDoSyncAndBackup =
  Programs.doSyncAndBackup
    <$> OA.strOption (OA.long "config-sync" <> OA.metavar "SYNC-CONFIG-FILE" <> OA.help "Path to synchronisation configuration file")
    <*> OA.strOption (OA.long "config-backup" <> OA.metavar "BACKUP-CONFIG-FILE" <> OA.help "Path to backup configuration file")


progDescSyncAndBackup :: OA.InfoMod a
progDescSyncAndBackup =
  OA.progDesc
    "Runs synchronisation and backup processes respectively. \
    \Note that you most probably do not want to be super user when running \
    \this command."
