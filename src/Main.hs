import Steam
import Data.List
    
main :: IO ()
main = do
  ids <- getSteamIDs
  putStrLn (unwords (map steamIdLegacy ids))
