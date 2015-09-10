{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Steam.Shortcuts
    (
     SteamShortcut(..),
     readShortcuts,
     writeShortcuts
    )
    where
import Steam
import Steam.BinVDF
import Data.Maybe
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types

data SteamShortcut = SteamShortcut {appName :: String, exe :: String, startDir :: String, icon :: String, tags :: [String]} deriving (Show)

instance FromJSON SteamShortcut where
    parseJSON = withObject "shortcut" $ \o ->
                do
                  appName  <- o .: "AppName"
                  exe      <- o .: "Exe"
                  startDir <- o .: "StartDir"
                  icon     <- o .: "icon"
                  tags     <- o .: "tags"
                  return SteamShortcut {..}

shortcuts :: Value -> Parser [SteamShortcut]
shortcuts =  withObject "x" $ \o -> o .: "Shortcuts"

readShortcuts :: SteamID -> IO (Maybe [SteamShortcut])
readShortcuts s = readShortcutsStr (shortcutFileLoc s)

readShortcutsStr :: String -> IO (Maybe [SteamShortcut])
readShortcutsStr s = do
  vdf <- readBinVDF s
  case vdf of
    Just n ->  return $ parseMaybe shortcuts =<< decode (encode n)
    Nothing -> return Nothing

writeShortcuts :: [SteamShortcut] -> IO ()
writeShortcuts s = undefined
