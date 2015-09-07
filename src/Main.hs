import Steam
import Data.List
    
main :: IO ()
main = do
  ids <- getSteamIDs
  putStrLn (intercalate " " (map (steamIdLegacy) ids))
