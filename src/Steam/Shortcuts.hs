{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Steam.Shortcuts
    (
     SteamShortcut(..),
     readShortcuts,
     writeShortcuts
    )
    where
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Text as T
import           Debug.Trace
import           Steam
import           Steam.BinVDF

data SteamShortcut = SteamShortcut {
      appName :: String,
      exe :: String,
      startDir :: String,
      icon :: String,
      tags :: [String],
      hidden :: Maybe Int,
      path :: Maybe String
    } deriving (Show)

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

instance FromJSON SteamShortcut where
    parseJSON =  withObject "shortcut" buildShortcut . jsonLower
        where buildShortcut o =
                do
                  appName  <- o .: "appname"
                  exe      <- o .: "exe"
                  startDir <- o .: "startdir"
                  icon     <- o .: "icon"
                  tags     <- o .: "tags"
                  path     <- o .:? "shortcutpath"
                  hidden   <- o .:? "hidden"
                  return SteamShortcut {..}

instance ToJSON SteamShortcut where
    toJSON s = object [
                "AppName"       .= appName s,
                "exe"           .= exe s,
                "StartDir"      .= startDir s,
                "icon"          .= icon s,
                "ShortcutPath"  .= Steam.Shortcuts.path s,
                "Hidden"        .= hidden s,
                "tags"          .= tags s
               ]

shortcuts :: Value -> Parser [SteamShortcut]
shortcuts =  withObject "x"  (.: "shortcuts") . jsonLower

readShortcuts :: SteamID -> IO (Maybe [SteamShortcut])
readShortcuts s = readShortcutsStr (shortcutFileLoc s)

readShortcutsStr :: String -> IO (Maybe [SteamShortcut])
readShortcutsStr s = do
  vdf <- readBinVDF s
  case vdf of
    Just n ->  return $ parseMaybe shortcuts n
    Nothing -> return Nothing

writeShortcuts :: SteamID -> [SteamShortcut] -> IO ()
writeShortcuts steamid = writeShortcutsStr (shortcutFileLoc steamid)

writeShortcutsStr :: FilePath -> [SteamShortcut] -> IO ()
writeShortcutsStr p shorts = writeBinVDF p $ Object $ HM.fromList [("Shortcuts", toJSON shorts)]
