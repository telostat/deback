module Deback.Cli where

import Control.Applicative ((<**>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deback.Programs (doListDisks)
import Deback.Smartctl
  ( Disk (..)
  , DiskDevice (..)
  , listDisks
  )
import qualified Options.Applicative as OA
import qualified Text.Layout.Table as Table


commandParser :: OA.Parser (IO ())
commandParser =
  OA.subparser
    ( OA.command "list-disks" (OA.info (pure doListDisks) (OA.progDesc listDisksProgDesc))
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
