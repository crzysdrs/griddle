import Steam
import Steam.Shortcuts
import Data.List
import Data.Maybe
    
main :: IO ()
main = do
  ids <- getSteamIDs
  shortcuts <- mapM readShortcuts ids
  let found = catMaybes shortcuts
  putStrLn (unlines (map show found))
