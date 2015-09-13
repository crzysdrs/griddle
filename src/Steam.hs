module Steam where
import Data.Bits
import System.Info
import System.FilePath
import System.Directory
import Text.Read
import Data.Maybe

data SteamUniverse = UnivInvalid | UnivPublic | UnivBeta |  UnivInternal | UnivDev deriving (Enum)
data SteamID = SteamID {universe :: SteamUniverse, accountType :: SteamInstance, accountId :: Int, path :: FilePath}
data SteamInstance = SteamInstance {char :: Char, typeId :: Int, defaultInst :: Int}

defaultUserData :: FilePath -> FilePath -> Maybe FilePath
defaultUserData home steam = case os of
                          "darwin" -> Just $ joinPath [home, "Library", "Application Support", "Steam", "userdata"]
                          "windows" -> Just $ joinPath  [steam, "userdata"]
                          "linux" -> Just $ joinPath [home, ".local", "share", "Steam", "userdata"]
                          _ -> Nothing

buildSteamID :: FilePath -> Maybe SteamID
buildSteamID path = do
  let strUserId = takeBaseName path
  actualId <- readMaybe strUserId :: Maybe Int
  Just SteamID {universe = UnivPublic, accountType = instanceInfo 'U', accountId = actualId, path = path}

getSteamIDs :: IO [SteamID]
getSteamIDs = do
  homeDir <- getHomeDirectory
  let userData = defaultUserData homeDir "steamdir"
  userDirs <- case userData of
                Nothing -> return []
                Just d -> do
                  subdirs <- getDirectoryContents d
                  return $ map (\l -> joinPath (d:[l])) subdirs
  let steamIds = map buildSteamID userDirs
  return $ catMaybes steamIds

-- https://developer.valvesoftware.com/wiki/SteamID
instanceInfo:: Char -> SteamInstance
instanceInfo c@'U' = SteamInstance {char=c, typeId=1, defaultInst=bit 0}
instanceInfo c@'M' = SteamInstance {char=c, typeId=2, defaultInst=0}
instanceInfo c@'G' = SteamInstance {char=c, typeId=3, defaultInst=bit 0}
instanceInfo c@'A' = SteamInstance {char=c, typeId=4, defaultInst=0}
instanceInfo c@'P' = SteamInstance {char=c, typeId=5, defaultInst=0}
instanceInfo c@'C' = SteamInstance {char=c, typeId=6, defaultInst=0}
instanceInfo c@'g' = SteamInstance {char=c, typeId=7, defaultInst=0}
instanceInfo c@'T' = SteamInstance {char=c, typeId=8, defaultInst=0}
instanceInfo c@'c' = SteamInstance {char=c, typeId=8, defaultInst=bit 19}
instanceInfo c@'L' = SteamInstance {char=c, typeId=8, defaultInst=bit 18}
--instanceInfo ?   = SteamInstance {char=c, typeId=9, defaultInst=0}
instanceInfo c@'a' = SteamInstance {char=c, typeId=10, defaultInst=0}
instanceInfo _     = SteamInstance {char='I', typeId=0, defaultInst=0}

mask :: (Num a, Bits a) => Int -> a
mask n = bit n - 1

steamId64 :: SteamID -> Int
steamId64 (SteamID {universe = u, accountType = at,  accountId = ai})
    = foldl (.|.) 0 [fromEnum u `shift` 56, typeId at `shift` 52, defaultInst at `shift` 32,  ai]

steamId3 :: SteamID -> String
steamId3 (SteamID {universe = u, accountType = at,  accountId = ai})
    = concat ["[", [char at], ":", show (fromEnum u), ":", show ai, "]"]

steamIdLegacy :: SteamID -> String
steamIdLegacy (SteamID {universe = u, accountType = at, accountId = ai})
    = concat ["STEAM_", show (fromEnum u - 1), ":", show (ai .&. bit 0), ":", show ((ai `shiftR` 1) .&. mask 31 )]

shortcutFileLoc :: SteamID -> FilePath
shortcutFileLoc s = joinPath [path s, "config", "shortcuts.vdf"]

steamGridDir :: SteamID -> FilePath
steamGridDir s = joinPath [path s, "config", "grid"]
