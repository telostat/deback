{-# LANGUAGE OverloadedStrings #-}

module Deback.Tools.Smartctl where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process.Typed as TP


newtype DeviceScanResult = DeviceScanResultDevices
  { deviceScanResultDevices :: [DiskDevice]
  }


instance Aeson.FromJSON DeviceScanResult where
  parseJSON = Aeson.withObject "DeviceScanResult" $ \o -> DeviceScanResultDevices <$> o .: "devices"


data DiskDevice = DiskDevice
  { diskDeviceName :: !T.Text
  , diskDeviceInfoName :: !T.Text
  , diskDeviceType :: !T.Text
  , diskDeviceProtocol :: !T.Text
  }
  deriving (Show)


instance Aeson.FromJSON DiskDevice where
  parseJSON = Aeson.withObject "Device" $ \o ->
    DiskDevice
      <$> o .: "name"
      <*> o .: "info_name"
      <*> o .: "type"
      <*> o .: "protocol"


data Disk = Disk
  { diskDevice :: !DiskDevice
  , diskModelName :: !(Maybe T.Text)
  , diskSerialNumber :: !(Maybe T.Text)
  , diskFirmwareVersion :: !(Maybe T.Text)
  , diskUserCapacity :: Integer
  }
  deriving (Show)


instance Aeson.FromJSON Disk where
  parseJSON = Aeson.withObject "Disk" $ \o ->
    Disk
      <$> o .: "device"
      <*> o .: "model_name"
      <*> o .: "serial_number"
      <*> o .: "firmware_version"
      <*> (o .: "user_capacity" >>= Aeson.withObject "DiskUserCapavity" (.: "bytes"))


listDiskDevices :: IO [DiskDevice]
listDiskDevices = deviceScanResultDevices <$> runSmartCtl ["--scan"]


listDisks :: IO [Disk]
listDisks = listDiskDevices >>= mapM (getDisk . diskDeviceName)


getDisk :: T.Text -> IO Disk
getDisk name = runSmartCtl ["-i", T.unpack name]


runSmartCtl :: Aeson.FromJSON a => [String] -> IO a
runSmartCtl args = do
  out <- TP.readProcessStdout_ process
  case Aeson.eitherDecode out of
    Left err -> hPutStrLn stderr ("Error while decoding smartctl output: " <> err) >> exitFailure
    Right sv -> pure sv
  where
    process = TP.proc "smartctl" ("--json" : args)
