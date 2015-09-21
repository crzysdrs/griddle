{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Steam.Shortcuts
    (
     SteamShortcut(..),
     readShortcuts,
     writeShortcuts,
     appid,
     VDFList(..),
     deparseShortcuts,
     parseShortcuts
    )
    where
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Bits
import           Data.Digest.CRC32
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Text as T
import           Text.Read
import           Data.Word
import           Debug.Trace
import           Steam
import           Steam.BinVDF
import Data.List
import qualified  Data.Vector as V
data SteamShortcut = SteamShortcut {
      appName :: String,
      exe :: String,
      startDir :: String,
      icon :: String,
      tags :: VDFList String,
      hidden :: Maybe Int,
      path :: Maybe String
    } deriving (Show, Eq)

data VDFList a = VDFList [a] deriving (Show, Eq)
appid :: SteamShortcut -> Word64
appid s =  ((bits32 .|. bit 31) `shiftL` 32) .|. bit 25
           where bits32 = fromIntegral (crc32 $ byteString (exe s ++ appName s))

byteString s = map (fromIntegral . fromEnum) s :: [Word8]

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

instance (FromJSON a) => FromJSON (VDFList a) where
  parseJSON = withObject "Vector" $ \o -> do
    let (keys, _) = unzip $ HM.toList o
        knums = do
          mapM (readMaybe .T.unpack) keys
        asc = [0..length keys - 1]
    case knums of
      Just ks -> if sort ks == asc
                 then do vals <- mapM ((.:) o . T.pack . show) asc
                         return $ VDFList vals
                 else fail ("Not consistent numbering VDF array")
      Nothing -> fail ("Not all numeric VDF array")

instance (ToJSON a) => ToJSON (VDFList a) where
    toJSON (VDFList xs) = object (zip (map (T.pack . show) [(0 :: Int)..]) (map toJSON xs))

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
readShortcuts s = readShortcutsFile (shortcutFileLoc s)

parseShortcuts :: Value -> Maybe [SteamShortcut]
parseShortcuts vdf = parseMaybe shortcuts vdf

readShortcutsFile :: String -> IO (Maybe [SteamShortcut])
readShortcutsFile s = do
  vdf <- readBinVDF s
  case vdf of
    Just n ->  return $ parseShortcuts n
    Nothing -> return Nothing

writeShortcuts :: SteamID -> [SteamShortcut] -> IO ()
writeShortcuts steamid = writeShortcutsFile (shortcutFileLoc steamid)

writeShortcutsFile :: FilePath -> [SteamShortcut] -> IO ()
writeShortcutsFile p shorts = writeBinVDF p $ deparseShortcuts shorts

deparseShortcuts :: [SteamShortcut] -> Value
deparseShortcuts shortcuts = Object $ HM.fromList [("Shortcuts", toJSON shortcuts)]
